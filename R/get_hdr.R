#' Computing the highest density regions of a 2D density
#'
#' `get_hdr` is used to estimate a 2-dimensional density and compute
#' corresponding HDRs. The estimated density and HDRs are represented in a
#' discrete form as a grid, defined by arguments `rangex`, `rangey`, and `n`.
#' `get_hdr` is used internally by layer functions `stat_hdr()`,
#' `stat_hdr_points()`, `stat_hdr_fun()`, etc.
#'
#' @param method Either a character (`"kde"`, `"mvnorm"`, `"histogram"`,
#'   `"freqpoly"`, or `"fun"`) or `method_*()` function. See the "The `method`
#'   argument" section below for details.
#' @param data A data frame with columns `x` and `y`.
#' @param probs Probabilities to compute HDRs for.
#' @param rangex,rangey Range of grid representing estimated density and HDRs,
#'   along the x- and y-axes.
#' @param n Resolution of grid representing estimated density and HDRs.
#' @param hdr_membership Should HDR membership of data points (`data`) be
#'   computed? Defaults to `TRUE`, although it is computationally expensive for
#'   large data sets.
#' @param fun Optional, a joint probability density function, must be vectorized
#'   in its first two arguments. See the "The `fun` argument" section below for
#'   details.
#' @param args Optional, a list of arguments to be provided to `fun`.
#'
#' @section The `method` argument: The density estimator used to estimate the
#'   HDRs is specified with the `method` argument. The simplest way to specify
#'   an estimator is to provide a character value to `method`, for example
#'   `method = "kde"` specifies a kernel density estimator. However, this
#'   specification is limited to the default behavior of the estimator.
#'
#'   Instead, it is possible to provide a function call, for example: `method =
#'   method_kde()`. In many cases, these functions accept parameters governing
#'   the density estimation procedure. Here, `method_kde()` accepts parameters
#'   `h` and `adjust`, both related to the kernel's bandwidth. For details, see
#'   `?method_kde`. Every method of bivariate density estimation implemented has
#'   such corresponding `method_*()` function, each with an associated help
#'   page.
#'
#'   Note: `geom_hdr()` and other layer functions also have `method` arguments
#'   which behave in the same way. For more details on the use and
#'   implementation of the `method_*()` functions, see `vignette("method",
#'   "ggdensity")`.
#'
#' @section The `fun` argument: If `method` is set to `"fun"`, `get_hdr()`
#'   expects a bivariate probability density function to be specified with the
#'   `fun` argument. It is required that `fun` be a function of at least two
#'   arguments (`x` and `y`). Beyond these first two arguments, `fun` can have
#'   arbitrarily many arguments; these can be set in `get_hdr()` as a named list
#'   via the `args` parameter.
#'
#'   Note: `get_hdr()` requires that `fun` be vectorized in `x` and `y`. For an
#'   example of an appropriate choice of `fun`, see the final example below.
#'
#' @returns
#'
#' `get_hdr` returns a list with elements `df_est` (`data.frame`), `breaks`
#' (named `numeric`), and `data` (`data.frame`).
#'
#' * `df_est`: the estimated HDRs and density evaluated on the grid defined by `rangex`, `rangey`, and `n`.
#' The column of estimated HDRs (`df_est$hdr`) is a numeric vector with values
#' from `probs`. The columns `df_est$fhat` and `df_est$fhat_discretized`
#' correspond to the estimated density on the original scale and rescaled to sum
#' to 1, respectively.
#'
#' * `breaks`: the heights of the estimated density (`df_est$fhat`) corresponding to the HDRs specified by `probs`.
#' Will always have additional element `Inf` representing the cutoff for the
#' 100% HDR.
#'
#' * `data`: the original data provided in the `data` argument.
#' If `hdr_membership` is set to `TRUE`, this includes a column
#' (`data$hdr_membership`) with the HDR corresponding to each data point.
#'
#' @examples
#' df <- data.frame(x = rnorm(1e3), y = rnorm(1e3))
#'
#' # Two ways to specify `method`
#' get_hdr(df, method = "kde")
#' get_hdr(df, method = method_kde())
#'
#' \dontrun{
#'
#' # If parenthesis are omitted, `get_hdr()` errors
#' get_hdr(df, method = method_kde)
#' }
#'
#' # Estimate different HDRs with `probs`
#' get_hdr(df, method = method_kde(), probs = c(.975, .6, .2))
#'
#' # Adjust estimator parameters with arguments to `method_kde()`
#' get_hdr(df, method = method_kde(h = 1))
#'
#' # Parametric normal estimator of density
#' get_hdr(df, method = "mvnorm")
#' get_hdr(df, method = method_mvnorm())
#'
#' # Compute "population" HDRs of specified bivariate pdf with `method = "fun"`
#' f <- function(x, y, sd_x = 1, sd_y = 1) dnorm(x, sd = sd_x) * dnorm(y, sd = sd_y)
#'
#' get_hdr(
#'   method = "fun", fun = f,
#'   rangex = c(-5, 5), rangey = c(-5, 5)
#'  )
#'
#' get_hdr(
#'   method = "fun", fun = f,
#'   rangex = c(-5, 5), rangey = c(-5, 5),
#'   args = list(sd_x = .5, sd_y = .5) # specify additional arguments w/ `args`
#' )
#'
#' @export
get_hdr <- function(data = NULL, method = "kde", probs = c(.99, .95, .8, .5), n = 100, rangex = NULL, rangey = NULL, hdr_membership = TRUE, fun, args = list()) {

  # Deal with missing data argument
  if (is.null(data)) {
    if (!is.character(method) | (is.character(method) && method != "fun")) {
      stop('`data` must be provided unless `method = "fun"`')
    } else {
      if (is.null(rangex) | is.null(rangey)) {
        stop('If `data` is unspecified, `rangex` and `rangey` must be provided when `method = "fun"`')
      }
    }
  }

  rangex <- rangex %||% range(data$x)
  rangey <- rangey %||% range(data$y)

  probs <- fix_probs(probs)

  # Create df_est (estimated density evaluated on a grid) depending on specified method:
  if (is.character(method) && method == "fun") {

    df_est <- f_est(method = NULL, n = n, rangex = rangex, rangey = rangey, fun = fun, args = args)

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

    # parse args of method to determine strategy of `method`
    method_formals <- names(formals(method))

    # If `data` is the only argument to `method`, we know `method`
    # is a function factory, returning a closure of pdf in terms of x, y:
    if (length(method_formals) == 1 && method_formals == "data") {

      df_est <- f_est(method, data, n, rangex, rangey)

    # Otherwise `method` computes a grid for us, shortcutting
    # representing pdf in terms of x, y:
    } else if (length(method_formals) == 4 && all(method_formals == c("data", "n", "rangex", "rangey"))) {

      df_est <- method(data, n, rangex, rangey)

    } else {

      stop("Invalid `method` argument -- did you forget the `()`?")

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
  df_est$hdr <- vapply(df_est$fhat, get_hdr_val, numeric(1), breaks, probs)

  # find hdr membership of points from data
  if (!is.null(data) & hdr_membership) {
    data$hdr_membership <- mapply(get_hdr_membership, data$x, data$y, MoreArgs = list(df_est, breaks, probs), SIMPLIFY = TRUE)
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

fix_probs <- function(probs) {
  stopifnot("Probabilities must be between 0 and 1, exclusive" = all(probs > 0) & all(probs < 1))

  sort(probs, decreasing = TRUE)
}

get_hdr_val <- function(fhat, breaks, probs) {
  hdrs <- which(fhat >= breaks)
  if (length(hdrs) == 0) return(1)
  probs[max(hdrs)]
}

get_hdr_membership <- function(x, y, df_est, breaks, probs) {
  df_est$dist <- (x - df_est$x)^2 + (y - df_est$y)^2
  fhat <- df_est[which.min(df_est$dist), "fhat"]

  get_hdr_val(fhat, breaks, probs)
}


# method is a function of data
# fun is a function of vectors x, y
f_est <- function(method, data, n, rangex, rangey, fun = NULL, args = list()) {

  # If `fun` isn't specified, method returns a closure
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


