# methods that return est pdf as closure  ---------------------------------

#' Univariate parametric normal HDR estimator
#'
#' Function used to specify univariate normal density estimator
#' for `get_hdr_1d()` and layer functions (e.g. `geom_hdr_rug()`).
#'
#' For more details on the use and implementation of the `method_*_1d()` functions,
#' see `vignette("method", "ggdensity")`.
#'
#' @examples
#' # Normal estimators are useful when an assumption of normality is appropriate
#' df <- data.frame(x = rnorm(1e3))
#'
#' ggplot(df, aes(x)) +
#'   geom_hdr_rug(method = method_norm_1d()) +
#'   geom_density()
#'
#' # Can also be used with `get_hdr_1d()` for numerical summary of HDRs
#' get_hdr_1d(df$x, method = method_norm_1d())
#'
#' @export
method_norm_1d <- function() {

  function(x) {

    mu_hat <- mean(x)
    sigma_hat <- sd(x)

    function(x) dnorm(x, mu_hat, sigma_hat)

  }
}

# methods that return closures that compute a grid ------------------------

#' Univariate kernel density HDR estimator
#'
#' Function used to specify univariate kernel density estimator
#' for `get_hdr_1d()` and layer functions (e.g. `geom_hdr_rug()`).
#'
#' For more details on the use and implementation of the `method_*_1d()` functions,
#' see `vignette("method", "ggdensity")`.
#'
#' @inheritParams stats::density
#'
#' @examples
#' df <- data.frame(x = rnorm(1e3, sd = 3))
#'
#' ggplot(df, aes(x)) +
#'   geom_hdr_rug(method = method_kde_1d()) +
#'   geom_density()
#'
#' # Details of the KDE can be adjusted with arguments to `method_kde_1d()`
#' ggplot(df, aes(x)) +
#'   geom_hdr_rug(method = method_kde_1d(adjust = 1/5)) +
#'   geom_density(adjust = 1/5)
#'
#' ggplot(df, aes(x)) +
#'   geom_hdr_rug(method = method_kde_1d(kernel = "triangular")) +
#'   geom_density(kernel = "triangular")
#'
#' # Can also be used with `get_hdr_1d()` for numerical summary of HDRs
#' get_hdr_1d(df$x, method = method_kde_1d())
#'
#' @export
method_kde_1d <- function(bw = "nrd0", adjust = 1, kernel = "gaussian", weights = NULL, window = kernel) {

  function(x, n, range) {

    nx <- length(x)

    if (is.null(weights)) {
      weights <- rep(1 / nx, nx)
    } else {
      weights <- normalize(weights)
    }

    dens <- stats::density(
      x,
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      weights = weights,
      window = window,
      n = n,
      from = range[1],
      to = range[2]
    )

    data.frame(
      x = dens$x,
      fhat = dens$y
    )

  }
}

#' Univariate histogram HDR estimator
#'
#' Function used to specify univariate histogram density estimator
#' for `get_hdr_1d()` and layer functions (e.g. `geom_hdr_rug()`).
#'
#' For more details on the use and implementation of the `method_*_1d()` functions,
#' see `vignette("method", "ggdensity")`.
#'
#' @param bins Number of bins. Defaults to normal reference rule (Scott, pg 59).
#'
#' @references Scott, David W. Multivariate Density Estimation (2e), Wiley.
#'
#' @examples
#' # Histogram estimators can be useful when data has boundary constraints
#' df <- data.frame(x = rexp(1e3))
#'
#' # Strip chart to visualize 1-d data
#' p <- ggplot(df, aes(x)) +
#'   geom_jitter(aes(y = 0), width = 0, height = 2) +
#'   scale_y_continuous(name = NULL, breaks = NULL) +
#'   coord_cartesian(ylim = c(-3, 3))
#'
#' p
#'
#' p + geom_hdr_rug(method = method_histogram_1d())
#'
#' # The resolution of the histogram estimator can be set via `bins`
#' p + geom_hdr_rug(method = method_histogram_1d(bins = 5))
#'
#' # Can also be used with `get_hdr_1d()` for numerical summary of HDRs
#' get_hdr_1d(df$x, method = method_histogram_1d())
#'
#' @export
method_histogram_1d <- function(bins = NULL) {

  function(x, n, range) {

    nx <- length(x)

    # Default to normal reference rule (Scott p. 59)
    if (is.null(bins)) {
      hx <- 3.504 * stats::sd(x) * nx^(-1/3)
      bins <- round((range[2] - range[1]) / hx)
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
}

#' Univariate frequency polygon HDR estimator
#'
#' Function used to specify univariate frequency polygon density estimator
#' for `get_hdr_1d()` and layer functions (e.g. `geom_hdr_rug()`).
#'
#' For more details on the use and implementation of the `method_*_1d()` functions,
#' see `vignette("method", "ggdensity")`.
#'
#' @inheritParams method_histogram_1d
#'
#' @references Scott, David W. Multivariate Density Estimation (2e), Wiley.
#'
#' @examples
#' df <- data.frame(x = rnorm(1e3))
#'
#' # Strip chart to visualize 1-d data
#' p <- ggplot(df, aes(x)) +
#'   geom_jitter(aes(y = 0), width = 0, height = 2) +
#'   scale_y_continuous(name = NULL, breaks = NULL) +
#'   coord_cartesian(ylim = c(-3, 3))
#'
#' p
#'
#' p + geom_hdr_rug(method = method_freqpoly_1d())
#'
#' # The resolution of the frequency polygon estimator can be set via `bins`
#' p + geom_hdr_rug(method = method_freqpoly_1d(bins = 100))
#'
#' # Can also be used with `get_hdr_1d()` for numerical summary of HDRs
#' get_hdr_1d(df$x, method = method_freqpoly_1d())
#'
#' @export
method_freqpoly_1d <- function(bins = NULL) {

  function(x, n, range) {

    # Start with output from method_histogram
    df <- method_histogram_1d(bins)(x, n, range)

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
}



