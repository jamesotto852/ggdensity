# parameters:
  # kde: bw, adjust, w
  # histogram: bins
  # freqpoly: bins
  # mvnorm:

#' Computing the highest density regions of a 2D density
#'
#' TODO - fill in with how to to use `method` as a string, function factory, and how to use `fun`.
#' Likely will need several sections to describe various uses.
#' Also need to expand on `parameters` parameter.
#'
#' @inheritParams get_hdr
#' @param x A vector of data
#' @param range Range to compute regions.
#' @param n Resolution of grid defined by range
#' @param fun Optional, a probability density function, must be vectorized in its first argument.
#'
#' @export
get_hdr_1d <- function(method = "kde", x, probs = c(.99, .95, .8, .5), n = 512, range, parameters = list(), HDR_membership = TRUE, fun, args = list()) {

  probs <- sort(probs, decreasing = TRUE)

  # Create df_est (estimated density evaluated on a grid) depending on specified method:
  if (is.character(method) && method == "fun") {

    df_est <- f_est_1d(method = NULL, x, probs, n, range, fun = fun, args = args)

  } else if (is.character(method)) {

    df_est <- switch(method,
      "kde" = kde_est_1d(x, n, range, parameters),
      "histogram" = histogram_est_1d(x, n, range, parameters),
      "freqpoly" = freqpoly_est_1d(x, n, range, parameters),
      "norm" = f_est_1d(method_norm_1d, x, n, range, parameters)
    )

    if (!method %in% c("kde", "histogram", "freqpoly", "norm")) stop("Invalid method specified")

  } else if (is.function(method)) {

    df_est <- f_est_1d(method, x, n, range, parameters)

  }

  # Manipulate df_est to get information about HDRs:

  # force estimate to integrate to 1
  df_est$fhat_discretized <- normalize(df_est$fhat)

  # temporarily rescale df$fhat for stability
  fhat_max <- max(df_est$fhat)
  df_est$fhat <- df_est$fhat / fhat_max

  # find cutoffs (in terms of rescaled fhat)
  breaks <- c(find_cutoff(df_est, probs), Inf)

  # find HDRs for points in the grid
  df_est$HDR <- vapply(df_est$fhat, get_hdr_val, numeric(1), breaks, probs)

  # find hdr membership of points from data
  if (!is.null(data) & HDR_membership) {
    HDR_membership <- vapply(x, get_hdr_membership_1d, numeric(1), df_est, breaks, probs)
  }

  # create data frame w/ input data (x) + HDR membership
  data <- data.frame(
    x = x,
    HDR_membership = HDR_membership
  )

  # transforming df_est$fhat and breaks back to original scale:
  df_est$fhat <- df_est$fhat * fhat_max
  breaks <- breaks * fhat_max

  # Give breaks nicely formatted names, corresponding to HDRs:
  names(breaks) <- scales::percent_format(accuracy = 1)(probs)

  # bundle everything together
  list(
    df_est = df_est,
    breaks = breaks,
    data = data
  )

}

get_hdr_membership_1d <- function(x, df_est, breaks, probs) {
  df_est$dist <- (x - df_est$x)^2
  fhat <- df_est[which.min(df_est$dist), "fhat"]

  get_hdr_val(fhat, breaks, probs)
}


kde_est_1d <- function(x, n, range, parameters) {

  nx <- length(x)

  if (is.null(parameters$adjust)) parameters$adjust <- 1

  if (is.null(parameters$bw)) parameters$bw <- "nrd0"

  if (is.null(parameters$w)) {
    parameters$w <- rep(1 / nx, nx)
  } else {
    parameters$w <- normalize(parameters$w)
  }

  # if less than 2 points return data frame of NAs and issue a warning
  if (nx < 2) {
    message("Groups with fewer than two data points have been dropped.")
    return(data.frame(
      x = NA_real_,
      fhat = NA_real_,
      fhat_discretized = NA_real_
    ))
  }

  dens <- stats::density(
    x,
    weights = parameters$w,
    bw = parameters$bw,
    adjust = parameters$adjust,
    kernel = parameters$kernel,
    n = n,
    from = range[1],
    to = range[2]
  )

  data.frame(
    x = dens$x,
    fhat = dens$y
  )

}

histogram_est_1d <- function(x, n, range, parameters) {
  # the n parameter is ignored here - no gridding

  bins <- parameters$bins
  nx <- length(x)

  # Default to normal reference rule (Scott p. 59)
  if (is.null(bins)) {
    hx <- 3.504 * stats::sd(x) * nx^(-1/3)
    bins <- round((range[2] - range[1]) / hx)
  }

  # if less than 2 points return data frame of NAs and a warning
  if (nx < 2) {
    message("Groups with fewer than two data points have been dropped.")
    return(data.frame(
      x = NA_real_,
      fhat = NA_real_,
      fhat_discretized = NA_real_
    ))
  }

  sx <- seq(range[1], range[2], length.out = bins + 1)
  de_x <- sx[2] - sx[1]
  midpts <- sx[-(bins+1)] + de_x/2
  n <- as.numeric(table(cut(x, sx)))

  data.frame(
    x = midpts,
    fhat = normalize(n)
  )
}

freqpoly_est_1d <- function(x, n = 512, range, parameters) {

  df <- histogram_est_1d(x, n, range, parameters)

  hx <- df$x[2] - df$x[1]

  # need to pad df from hist_marginal() w/ bins that have est prob of 0
  # so that we can interpolate
  df <- rbind(

    # add initial bin w/ est prob of 0
    data.frame(
      x = min(df$x) - hx,
      fhat = 0
    ),

    # include original histogram estimator
    df,

    # add final bin w/ est prob of 0
    data.frame(
      x = max(df$x) + hx,
      fhat = 0
    )

  )

  sx <- seq(range[1], range[2], length.out = n)

  interpolate_fhat <- function(x) {
    lower_x <- df$x[max(which(df$x < x))]
    upper_x <- df$x[min(which(df$x >= x))]

    lower_fhat <- df$fhat[max(which(df$x < x))]
    upper_fhat <- df$fhat[min(which(df$x >= x))]

    lower_fhat + (x - lower_x) * (upper_fhat - lower_fhat) / (upper_x - lower_x)
  }

  dens <- vapply(sx, interpolate_fhat, numeric(1))

  data.frame(
    x = sx,
    fhat = dens
  )

}

# method is a function of data vector x
# fun is a function of vector x -- the grid
# Might need to be more careful w/ axis transformations here
# TODO - implement geom_hdr_rug_fun()
f_est_1d <- function(method, x, n, range, parameters = list(), fun = NULL, args = list()) {

  # If fun isn't specified, method returns a closure
  # representing closed form of density estimate
  fun <- fun %||% method(x, parameters)

  # grid to evaluate fun
  df <- data.frame(x = seq(range[1], range[2], length.out = n))

  # evaluate method on the grid, f required to be vectorized in x, y:
  df$fhat <- do.call(fun, c(quote(df$x), args))

  df

}

# TODO - document these, along with the *_est functions from above

#' @export
method_norm_1d <- function(x, parameters) {

  mu_hat <- mean(x)
  sigma_hat <- sd(x)

  function(x) dnorm(x, mu_hat, sigma_hat)

}

#' @export
method_exp_1d <- function(x, parameters) {

  lambda_hat <- mean(x)
  lambda_hat <- 1/lambda_hat

  function(x) dexp(x, lambda_hat)

}


