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
get_hdr_1d <- function(x, method = "kde", probs = c(.99, .95, .8, .5), n = 512, range = NULL, hdr_membership = TRUE, fun, args = list()) {

  # TODO consider expanding rangex/rangey by default
  range <- range %||% range(x)

  probs <- fix_probs(probs)

  # Create df_est (estimated density evaluated on a grid) depending on specified method:
  if (is.character(method) && method == "fun") {

    df_est <- f_est_1d(method = NULL, x = x, n, range = range, fun = fun, args = args)

  } else  {

    if (is.character(method)) {

      if (!method %in% c("kde", "norm", "histogram", "freqpoly")) stop("Invalid method specified")

      # If method is provided as a character, re-assign correct function output:
      method <- switch(method,
        "kde"       = method_kde_1d(),
        "histogram" = method_histogram_1d(),
        "freqpoly"  = method_freqpoly_1d(),
        "norm"      = method_norm_1d()
      )

    }

    # parse args of method to determine strategy of `method`
    method_formals <- names(formals(method))

    # If `data` is the only argument to `method`, we know `method`
    # is a function factory, returning a closure of pdf in terms of x, y:
    if (length(method_formals) == 1 && method_formals %in% c("x", "y")) {

      df_est <- f_est_1d(method, x, n, range)

    # Otherwise `method` computes a grid for us, shortcutting
    # representing pdf in terms of x, y:
    } else if (length(method_formals) == 3 && method_formals[1] %in% c("x", "y") & all(method_formals[2:3] == c("n", "range"))) {

      df_est <- method(x, n, range)

    } else {

      stop("Invalid `method` argument -- did you forget the `()`?")

    }

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
  df_est$hdr <- vapply(df_est$fhat, get_hdr_val, numeric(1), breaks, probs)

  # find hdr membership of points from data
  if (!is.null(x) & hdr_membership) {

    data <- data.frame(x = x)

    if (hdr_membership) {

      hdr_membership <- vapply(x, get_hdr_membership_1d, numeric(1), df_est, breaks, probs)

      # create data frame w/ input data (x) + HDR membership
      data$hdr_membership <- hdr_membership

    }

  } else {

    data <- NULL

  }

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

# method is a function of data vector x
# fun is a function of vector x -- the grid
# Might need to be more careful w/ axis transformations here
f_est_1d <- function(method, x, n, range, fun = NULL, args = list()) {

  # If fun isn't specified, method returns a closure
  # representing closed form of density estimate
  fun <- fun %||% method(x)

  # grid to evaluate fun
  df <- data.frame(x = seq(range[1], range[2], length.out = n))

  # evaluate method on the grid, f required to be vectorized in x, y:
  # (args is only non-empty if fun was specified)
  df$fhat <- do.call(fun, c(quote(df$x), args))

  df

}


