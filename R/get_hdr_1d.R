#' Computing the highest density regions of a 1D density
#'
#' temp
#'
#' @inheritParams get_hdr
#' @param method Either a character (`"kde"`, `"norm"`, `"histogram"`, `"freqpoly"`, or `"fun"`) or `method_*_1d()` function.
#'   See the "The `method` argument" section below for details.
#' @param x A vector of data
#' @param range Range to compute regions.
#' @param n Resolution of grid defined by range
#' @param fun Optional, a probability density function, must be vectorized in its first argument.
#'   See the "The `fun` argument" section below for details.
#'
#' @section The `method` argument:
#' The density estimator used to estimate the HDRs is specified with the `method` argument.
#' The simplest way to specify an estimator is to provide a character value to `method`,
#' for example `method = "kde"` specifies a kernel density estimator.
#' However, this specification is limited to the default behavior of the estimator.
#'
#' Instead, it is possible to provide a function call, for example: `method = method_kde_1d()`.
#' This is slightly different from the function calls provided in `get_hdr()`, note the `_1d` suffix.
#' In many cases, these functions accept parameters governing the density estimation procedure.
#' Here, `method_kde_1d()` accepts several parameters related to the choice of kernel.
#' For details, see `?method_kde_1d`.
#' Every method of univariate density estimation implemented has such corresponding `method_*_1d()` function,
#' each with an associated help page.
#'
#' Note: `geom_hdr_rug()` and other layer functions also have `method` arguments which behave in the same way.
#' For more details on the use and implementation of the `method_*_1d()` functions,
#' see `vignette("method", "ggdensity")`.
#'
#' @section The `fun` argument:
#' If `method` is set to `"fun"`, `get_hdr_1d()` expects a bivariate probability
#' density function to be specified with the `fun` argument.
#' It is required that `fun` be a function of at least one arguments (`x`).
#' Beyond this first argument, `fun` can have arbitrarily many arguments;
#' these can be set in `get_hdr_1d()` as a named list with `args`.
#'
#' Note: `get_hdr_1d()` requires that `fun` be vectorized in `x`.
#' For an example of an appropriate choice of `fun`, see the final example below.
#'
#' @returns
#'
#' `get_hdr_1d` returns a list with elements `df_est` (`data.frame`), `breaks` (named `numeric`), and `data` (`data.frame`).
#'
#' * `df_est`: the estimated HDRs and density evaluated on the grid defined by `range` and `n`.
#' The column of estimated HDRs (`$hdr`) is a numeric vector with values from `probs`.
#' The columns `$fhat` and `$fhat_discretized` correspond to the estimated density
#' on the original scale and normalized to sum to 1, respectively.
#'
#' * `breaks`: the heights of the estimated density (`$fhat`) corresponding to the HDRs specified by `probs`.
#' Will always have additional element `Inf` representing the cutoff for the 100% HDR.
#'
#' * `data`: the original data provided in the `data` argument.
#' If `hdr_membership` is set to `TRUE`, this includes a column (`$hdr_membership`)
#' with the HDR corresponding to each data point.
#'
#' @examples
#' x <- rnorm(1e3)
#'
#' # two ways to specify `method`
#' get_hdr_1d(x, method = "kde")
#' get_hdr_1d(x, method = method_kde_1d())
#'
#' \dontrun{
#'
#' # if parenthesis are omitted, `get_hdr_1d()` errors
#' get_hdr_1d(df, method = method_kde_1d)
#'
#' # if the `_1d` suffix is omitted, `get_hdr_1d()` errors
#' get_hdr_1d(x, method = method_kde())
#' }
#'
#' # adjust estimator parameters with arguments to `method_kde_1d()`
#' get_hdr_1d(x, method = method_kde_1d(kernel = "triangular"))
#'
#' # estimate different HDRs with `probs`
#' get_hdr_1d(x, method = method_kde_1d(), probs = c(.975, .6, .2))
#'
#' # "population" HDRs of specified univariate pdf with `method = "fun"`
#' f <- function(x, sd = 1) dnorm(x, sd = sd)
#' get_hdr_1d(method = "fun", fun = f, range = c(-5, 5))
#' get_hdr_1d(method = "fun", fun = f, range = c(-5, 5), args = list(sd = .5))
#'
#'
#' @export
get_hdr_1d <- function(x = NULL, method = "kde", probs = c(.99, .95, .8, .5), n = 512, range = NULL, hdr_membership = TRUE, fun, args = list()) {

  # Deal with missing data argument
  if (is.null(x)) {
    if (!is.character(method) | (is.character(method) && method != "fun")) {
      stop('`x` must be provided unless `method = "fun"`')
    } else {
      if (is.null(range)) {
        stop('If `x` is unspecified, `range` must be provided when `method = "fun"`')
      }
    }
  }

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

    } else if ("data" %in% method_formals) {

      stop("Invalid `method` argument -- did you forget the `_1d()`?")

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


