# methods that return est pdf as closure  ---------------------------------

#' Bivariate parametric normal HDR estimator
#'
#' temp
#'
#' For more details on the use and implementation of the `method_*()` functions,
#' see `vignette("method", "ggdensity")`.
#'
#' @examples
#' # Normal estimator is useful when an assumption of normality is appropriate
#' set.seed(1)
#' df <- data.frame(x = rnorm(1e3), y = rnorm(1e3))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_hdr(method = method_mvnorm(), xlim = c(-4, 4), ylim = c(-4, 4)) +
#'   geom_point(size = 1)
#'
#' # Can also be used with `get_hdr()` for numerical summary of HDRs
#' get_hdr(df, method = method_mvnorm())
#'
#'
#' @export
method_mvnorm <- function() {

  function(data) {

    data_matrix <- with(data, cbind(x, y))
    mu_hat <- colMeans(data_matrix)
    S <- cov(data_matrix)
    SInv <- solve(S)

    # TODO - I think something's off here, statistically:
    dmvnorm <- function(x, y) {
      Mdist <- t(c(x, y) - mu_hat) %*% SInv %*% (c(x, y) - mu_hat)
      dchisq(Mdist, 2)
    }

    function(x, y) mapply(dmvnorm, x, y)

  }

}


# methods that return closures that compute a grid ------------------------

#' Bivariate kernel density HDR estimator
#'
#' temp
#'
#' For more details on the use and implementation of the `method_*()` functions,
#' see `vignette("method", "ggdensity")`.
#'
#' @inheritParams ggplot2::stat_density2d
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(x = rnorm(1e3, sd = 3), y = rnorm(1e3, sd = 3))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_hdr(method = method_kde()) +
#'   geom_point(size = 1)
#'
#' # The defaults of `method_kde()` are the same as the estimator for `ggplot2::geom_density_2d()`
#' ggplot(df, aes(x, y)) +
#'   geom_density_2d_filled() +
#'   geom_hdr_lines(method = method_kde(), probs = seq(.1, .9, by = .1)) +
#'   theme(legend.position = "none")
#'
#' # The bandwidth of the estimator can be set directly with `h` or scaled with `adjust`
#' ggplot(df, aes(x, y)) +
#'   geom_hdr(method = method_kde(h = 1)) +
#'   geom_point(size = 1)
#'
#' ggplot(df, aes(x, y)) +
#'   geom_hdr(method = method_kde(adjust = 1/2)) +
#'   geom_point(size = 1)
#'
#' # Can also be used with `get_hdr()` for numerical summary of HDRs
#' get_hdr(df, method = method_kde())
#'
#'
#' @export
method_kde <- function(h = NULL, adjust = c(1, 1)) {

  function(data, n, rangex, rangey) {

    if (is.null(h)) {
      h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
    }

    h <- h * adjust

    kdeout <- MASS::kde2d(
      x = data$x, y = data$y, n = n, h = h,
      lims = c(rangex, rangey)
    )

    df <- with(kdeout, expand.grid("x" = x, "y" = y))
    df$fhat <- as.vector(kdeout$z)

    df

  }
}

#' Bivariate histogram HDR estimator
#'
#' temp
#'
#' For more details on the use and implementation of the `method_*()` functions,
#' see `vignette("method", "ggdensity")`.
#'
#' @param bins Number of bins along each axis.
#'   Either a vector of length 2 or a scalar value which is recycled for both dimensions.
#'   Defaults to normal reference rule (Scott, pg 87).
#' @param smooth If `TRUE`, HDRs are smoothed by the marching squares algorithm.
#' @param nudgex,nudgey Horizontal and vertical rules for choosing witness points when `smooth == TRUE`.
#'   Accepts character vector: `"left"`, `"none"`, `"right"` (`nudgex`) or  `"down"`, `"none"`, `"up"` (`nudgey`).
#'
#' @references Scott, David W. Multivariate Density Estimation (2e), Wiley.
#'
#' @examples
#' # Histogram estimators can be useful when data has boundary constraints
#' set.seed(1)
#' df <- data.frame(x = rexp(1e3), y = rexp(1e3))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_hdr(method = method_histogram()) +
#'   geom_point(size = 1)
#'
#' # The resolution of the histogram estimator can be set via `bins`
#' ggplot(df, aes(x, y)) +
#'   geom_hdr(method = method_histogram(bins = c(8, 25))) +
#'   geom_point(size = 1)
#'
#' # By setting `smooth = TRUE`, we can graphically smooth the "blocky" HDRs
#' ggplot(df, aes(x, y)) +
#'   geom_hdr(method = method_histogram(smooth = TRUE)) +
#'   geom_point(size = 1)
#'
#' # However, we need to set `nudgex` and `nudgey` to align the HDRs correctly
#' ggplot(df, aes(x, y)) +
#'   geom_hdr(method = method_histogram(smooth = TRUE, nudgex = "left", nudgey = "down")) +
#'   geom_point(size = 1)
#'
#' # Can also be used with `get_hdr()` for numerical summary of HDRs
#' get_hdr(df, method = method_histogram())
#'
#'
#' @export
method_histogram <- function(bins = NULL, smooth = FALSE, nudgex = "none", nudgey = "none") {

  # n is an argument, but it is not used
  function(data, n, rangex, rangey) {

    if (is.null(bins)) {
      bins <- numeric(2)

      # define histogram mesh according to Scott p. 87
      rho <- cor(data$x, data$y)
      hx <- 3.504 * sd(data$x) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
      hy <- 3.504 * sd(data$y) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
      bins[1] <- round((rangex[2] - rangex[1]) / hx)
      bins[2] <- round((rangey[2] - rangey[1]) / hy)

    } else if (length(bins == 1)) {
      bins <- rep(bins, 2)
    }

    xvals <- data$x
    yvals <- data$y

    xbtwn <- (rangex[1] <= xvals & xvals <= rangex[2])
    if (!all(xbtwn)) {
      xvals <- xvals[xbtwn]
      yvals <- yvals[xbtwn]
    }

    ybtwn <- (rangey[1] <= yvals & yvals <= rangey[2])
    if (!all(ybtwn)) {
      xvals <- xvals[ybtwn]
      yvals <- yvals[ybtwn]
    }

    sx <- seq(rangex[1], rangex[2], length.out = bins[1] + 1)
    sy <- seq(rangey[1], rangey[2], length.out = bins[2] + 1)
    de_x <- sx[2] - sx[1]
    de_y <- sy[2] - sy[1]
    box_area <- de_x * de_y

    xbin_mdpts <- sx[-(bins[1]+1)] + de_x/2
    ybin_mdpts <- sy[-(bins[2]+1)] + de_y/2

    xleft <- sx[-(bins[1]+1)]
    xright <- sx[-1]

    ybottom <- sy[-(bins[2]+1)]
    ytop <- sy[-1]


    df_cuts <- data.frame("xbin" = cut(xvals, sx), "ybin" = cut(yvals, sy))

    df <- with(df_cuts, expand.grid("xbin" = levels(xbin), "ybin" = levels(ybin)))
    df$n <- with(df_cuts, as.vector(table(xbin, ybin)))

    df$xbin_midpt <- xbin_mdpts[as.integer(df$xbin)]
    df$ybin_midpt <- ybin_mdpts[as.integer(df$ybin)]

    df$xmin <- df$xbin_midpt - de_x/2
    df$xmax <- df$xbin_midpt + de_x/2
    df$de_x <- de_x

    df$ymin <- df$ybin_midpt - de_y/2
    df$ymax <- df$ybin_midpt + de_y/2
    df$de_y <- de_y

    df$fhat <- with(df, n / (sum(n) * box_area))


    if (smooth) {

      if(nudgex == "left") df$x <- df$xmin
      if(nudgex == "none") df$x <- df$xbin_midpt
      if(nudgex == "right") df$x <- df$xmax

      if(nudgey == "down") df$y <- df$ymin
      if(nudgey == "none") df$y <- df$ybin_midpt
      if(nudgey == "up") df$y <- df$ymax

    } else {

      # No nudging if not smoothing
      df$x <- df$xbin_midpt
      df$y <- df$ybin_midpt

      # Evaluate histogram on a grid
      # For xyz_to_iso* funs, need tightly packed values for good isobands/lines
      # k*k points per histogram footprint
      # Higher values of k -> better visuals, more computationally expensive

      # Currently determining k heuristically - not based on any theoretical results
      # The necessary value of k seems to be O((bins[1]*bins[2])^(-1/3))
      # found constant which yields k = 50 for bins[1]*bins[2] = 10^2
      k <- if (bins[1] * bins[2] > 10^2) max(floor(225/((bins[1] * bins[2])^(1/3))), 5) else 50

      bbins <- bins * k

      ssx <- seq(rangex[1], rangex[2], length.out = bbins[1])
      ssy <- seq(rangey[1], rangey[2], length.out = bbins[2])

      ddf <- expand.grid(x = ssx, y = ssy)

      # Need fhat repeated in very particular way for grid:
      #   e.g.
      #      k = 2
      #      df$fhat = 1, 2,
      #                3, 4
      #     ddf$fhat = 1, 1, 2, 2,
      #                1, 1, 2, 2,
      #                3, 3, 4, 4,
      #                3, 3, 4, 4

      # m <- matrix(df$fhat, nrow = bins[2], byrow = TRUE)
      # ddf$fhat <- as.vector(kronecker(m, matrix(1, k, k)))

      fhat <- split(df$fhat, factor(rep(1:bins[2], each = bins[1]))) # split into rows
      fhat <- lapply(fhat, function(x) rep(x, each = k)) # repeat within rows (horizontal)
      fhat <- lapply(fhat, function(x) rep(x, times = k)) # repeat rows (vertical)
      fhat <- unlist(fhat) # concatenate
      ddf$fhat <- fhat

      df <- ddf
    }

    df[c("x", "y", "fhat")]

  }
}

#' Bivariate frequency polygon HDR estimator
#'
#' temp
#'
#' For more details on the use and implementation of the `method_*()` functions,
#' see `vignette("method", "ggdensity")`.
#'
#' @inheritParams method_histogram
#'
#' @references Scott, David W. Multivariate Density Estimation (2e), Wiley.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(x = rnorm(1e3), y = rnorm(1e3))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_hdr(method = method_freqpoly()) +
#'   geom_point(size = 1)
#'
#' # The resolution of the frequency polygon estimator can be set via `bins`
#' ggplot(df, aes(x, y)) +
#'   geom_hdr(method = method_freqpoly(bins = c(8, 25))) +
#'   geom_point(size = 1)
#'
#' # Can also be used with `get_hdr()` for numerical summary of HDRs
#' get_hdr(df, method = method_freqpoly())
#'
#'
#' @export
method_freqpoly <- function(bins = NULL) {

  # n is an argument, but it is not used
  function(data, n, rangex, rangey) {

    if (is.null(bins)) {
      bins <- numeric(2)

      # define histogram mesh according to Scott p. 87
      # TODO: fill in with rules for frequency polygons
      rho <- cor(data$x, data$y)
      hx <- 3.504 * sd(data$x) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
      hy <- 3.504 * sd(data$y) * (1 - rho^2)^(3/8) * nrow(data)^(-1/4)
      bins[1] <- round((rangex[2] - rangex[1]) / hx)
      bins[2] <- round((rangey[2] - rangey[1]) / hy)

    } else {
      if (length(bins == 1)) bins <- rep(bins, 2)
    }

    xvals <- data$x
    yvals <- data$y

    xbtwn <- (rangex[1] <= xvals & xvals <= rangex[2])
    if (!all(xbtwn)) {
      xvals <- xvals[xbtwn]
      yvals <- yvals[xbtwn]
    }

    ybtwn <- (rangey[1] <= yvals & yvals <= rangey[2])
    if (!all(ybtwn)) {
      xvals <- xvals[ybtwn]
      yvals <- yvals[ybtwn]
    }


    de_x <- (rangex[2] - rangex[1]) / bins[1]
    de_y <- (rangey[2] - rangey[1]) / bins[2]
    rangex[1] <- rangex[1] - de_x
    rangex[2] <- rangex[2] + de_x
    rangey[1] <- rangey[1] - de_y
    rangey[2] <- rangey[2] + de_y
    bins <- bins + 2
    sx <- seq(rangex[1], rangex[2], length.out = bins[1] + 1)
    sy <- seq(rangey[1], rangey[2], length.out = bins[2] + 1)


    box_area <- de_x * de_y

    xbin_mdpts <- sx[-(bins[1]+1)] + de_x/2
    ybin_mdpts <- sy[-(bins[2]+1)] + de_y/2

    xleft <- sx[-(bins[1]+1)]
    xright <- sx[-1]

    ybottom <- sy[-(bins[2]+1)]
    ytop <- sy[-1]


    df_cuts <- data.frame("xbin" = cut(xvals, sx), "ybin" = cut(yvals, sy))

    df <- with(df_cuts, expand.grid("xbin" = levels(xbin), "ybin" = levels(ybin)))
    df$n <- with(df_cuts, as.vector(table(xbin, ybin)))

    df$xbin_midpt <- xbin_mdpts[as.integer(df$xbin)]
    df$ybin_midpt <- ybin_mdpts[as.integer(df$ybin)]

    df$xmin <- df$xbin_midpt - de_x/2
    df$xmax <- df$xbin_midpt + de_x/2
    df$de_x <- de_x

    df$ymin <- df$ybin_midpt - de_y/2
    df$ymax <- df$ybin_midpt + de_y/2
    df$de_y <- de_y

    df$fhat <- with(df, n / (sum(n) * box_area))
    df$fhat_discretized <- normalize(df$fhat)

    grid <- expand.grid(
      x = sx[2:bins[1]],
      y = sy[2:bins[2]]
    )

    x_midpts <- unique(df$xbin_midpt)
    y_midpts <- unique(df$ybin_midpt)

    find_A <- function(coords) {
      x <- coords[[1]]
      y <- coords[[2]]

      row <- data.frame(
        x1 = max(x_midpts[x_midpts - x < 0]),
        x2 = min(x_midpts[x_midpts - x >= 0]),
        y1 = max(y_midpts[y_midpts - y < 0]),
        y2 = min(y_midpts[y_midpts - y >= 0])
      )

      row$fQ11 <- df[df$xbin_midpt == row$x1 & df$ybin_midpt == row$y1, "fhat"]
      row$fQ21 <- df[df$xbin_midpt == row$x2 & df$ybin_midpt == row$y1, "fhat"]
      row$fQ12 <- df[df$xbin_midpt == row$x1 & df$ybin_midpt == row$y2, "fhat"]
      row$fQ22 <- df[df$xbin_midpt == row$x2 & df$ybin_midpt == row$y2, "fhat"]

      xy_mat <- with(row, matrix(c(
        x2 * y2, -x2 * y1, -x1 * y2, x1 * y1,
        -y2, y1, y2, -y1,
        -x2, x2, x1, -x1,
        1, -1, -1, 1
      ), nrow = 4, byrow = TRUE))

      A <- with(row,
        1 / ((x2 - x1) * (y2 - y1)) * xy_mat %*% c(fQ11, fQ12, fQ21, fQ22)
      )

      row$a00 <- A[1]
      row$a10 <- A[2]
      row$a01 <- A[3]
      row$a11 <- A[4]

      row
    }


    A_list <- apply(grid, 1, find_A, simplify = FALSE)
    df_A <- do.call(rbind, A_list)

    coeffs_to_surface <- function(row, k) {
      sx <- seq(row[["x1"]], row[["x2"]], length.out = k)[-k]
      sy <- seq(row[["y1"]], row[["y2"]], length.out = k)[-k]

      fit <- function(x, y) row[["a00"]] + row[["a10"]] * x + row[["a01"]] * y + row[["a11"]] * x * y

      df <- expand.grid(x = sx, y = sy)
      df$fhat <- fit(df$x, df$y)

      df
    }


    # Currently determining k heuristically - not based on any theoretical results
    # The necessary value of k seems to be O((bins[1]*bins[2])^(-1/4))
    k <- if (bins[1] * bins[2] > 10^2) max(floor(30/((bins[1] * bins[2])^(1/4))), 3) else 10

    surface_list <- apply(df_A, 1, coeffs_to_surface, k, simplify = FALSE)
    df <- do.call(rbind, surface_list)

    df[c("x","y","fhat")]

  }
}

