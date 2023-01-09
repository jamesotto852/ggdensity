# methods that return est pdf as closure  ---------------------------------

#' Univariate parametric normal HDR estimator
#'
#' temp
#'
#' For more details on the use and implementation of the `method_*_1d()` functions,
#' see `vignette("method", "ggdensity")`.
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
#' temp
#'
#' For more details on the use and implementation of the `method_*_1d()` functions,
#' see `vignette("method", "ggdensity")`.
#'
#' @inheritParams stats::density
#'
#' @export
method_kde_1d <- function(bw = "nrd0", adjust = 1, kernel = "gaussian", weights = NULL) {

  function(x, n, range) {

    nx <- length(x)

    if (is.null(weights)) {
      weights <- rep(1 / nx, nx)
    } else {
      weights <- normalize(weights)
    }

    dens <- stats::density(
      x,
      weights = weights,
      bw = bw,
      adjust = adjust,
      kernel = kernel,
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
#' temp
#'
#' For more details on the use and implementation of the `method_*_1d()` functions,
#' see `vignette("method", "ggdensity")`.
#'
#' @param bins Number of bins. Defaults to normal reference rule (Scott, pg 59).
#'
#' @references Scott, David W. Multivariate Density Estimation (2e), Wiley.
#'
#' @export
method_histogram_1d <- function(bins = NULL) {

  parameters <- list(bins = bins)

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
#' temp
#'
#' For more details on the use and implementation of the `method_*_1d()` functions,
#' see `vignette("method", "ggdensity")`.
#'
#' @inheritParams method_histogram_1d
#'
#' @references Scott, David W. Multivariate Density Estimation (2e), Wiley.
#'
#' @export
method_freqpoly_1d <- function(bins = NULL) {

  parameters <- list(bins = bins)

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



