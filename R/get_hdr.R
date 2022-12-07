# parameters:
  # kde: h, adjust
  # histogram: bins, nudgex, nudgey, smooth
  # freqpoly: bins
  # mvnorm:

#' Computing the highest density regions of a 2D density
#'
#' TODO - fill in with how to to use `method` as a string, function factory, and how to use `fun`.
#' Likely will need several sections to describe various uses.
#' Also need to expand on `parameters` parameter.
#'
#' @param method temp
#' @param data A data frame with columns `x` and `y`
#' @param probs Probabilities to compute highest density regions for.
#' @param rangex,rangey Range to compute and draw regions.
#' @param n Resolution of grid defined by `xlim` and `ylim`.
#' @param HDR_membership Should HDR membership of data points be calculated?
#'   Defaults to `TRUE`, although it is computationally expensive.
#' @param fun Optional, a joint probability density function, must be vectorized in its first two arguments.
#' @param args Optional, a list of arguments to be provided to `fun`.
#'
#' @export
get_hdr <- function(data, method = "kde", probs = c(.99, .95, .8, .5), n = 100, rangex = NULL, rangey = NULL, HDR_membership = TRUE, fun, args = list()) {

  # TODO consider expanding rangex/rangey by default
  rangex <- rangex %||% range(data$x)
  rangey <- rangey %||% range(data$y)

  # TODO default expand rangex and rangey

  probs <- sort(probs, decreasing = TRUE)

  # Create df_est (estimated density evaluated on a grid) depending on specified method:
  if (is.character(method) && method == "fun") {

    df_est <- f_est(method = NULL, data = data, n, rangex, rangey, fun = fun, args = args)

  } else  {

    if (is.character(method)) {

      if (!method %in% c("kde", "mvnorm", "histogram", "freqpoly")) stop("Invalid method specified")

      # If method is provided as a character, re-assign correct function output:
      method <- switch(method,
        "kde"       = method_kde(),
        "histogram" = method_histogram(),
        "freqpoly"  = method_freqpoly(),
        "mvnorm"    = method_mvnorm()
      )

    }

    # TODO: Check that method is "right"

    # parse args of method to determine strategy of `method`
    method_formals <- names(formals(method))

    # If `data` is the only argument to `method`, we know `method`
    # is a function factory, returning a closure of pdf in terms of x, y:
    if (length(method_formals) == 1 && method_formals == "data") {

      df_est <- f_est(method, data, n, rangex, rangey)

    # Otherwise `method` computes a grid for us, shortcutting
    # representing pdf in terms of x, y:
    } else {

      df_est <- method(data, n, rangex, rangey)

    }

  }


  # remove unneeded attributes
  attr(df_est, "out.attrs") <- NULL

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
    data$HDR_membership <- mapply(get_hdr_membership, data$x, data$y, MoreArgs = list(df_est, breaks, probs), SIMPLIFY = TRUE)
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

get_hdr_val <- function(fhat, breaks, probs) {
  HDRs <- which(fhat >= breaks)
  if (length(HDRs) == 0) return(1)
  probs[max(HDRs)]
}

get_hdr_membership <- function(x, y, df_est, breaks, probs) {
  df_est$dist <- (x - df_est$x)^2 + (y - df_est$y)^2
  fhat <- df_est[which.min(df_est$dist), "fhat"]

  get_hdr_val(fhat, breaks, probs)
}


# method is a function of data
# fun is a function of vectors x, y
# Might need to be more careful w/ axis transformations here
f_est <- function(method, data, n, rangex, rangey, fun = NULL, args = list()) {

  # If fun isn't specified, method returns a closure
  # representing closed form of density estimate
  fun <- fun %||% method(data)

  # grid to evaluate fun
  df <- expand.grid(
    "x" = seq(rangex[1], rangex[2], length.out = n),
    "y" = seq(rangey[1], rangey[2], length.out = n)
  )

  # evaluate method on the grid, f required to be vectorized in x, y:
  # (args is only non-empty if fun was specified)
  df$fhat <- do.call(fun, c(quote(df$x), quote(df$y), args))

  df

}


