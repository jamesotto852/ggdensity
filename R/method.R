# TODO - document these

# methods that return est pdf as closure  ---------------------------------

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

#' @export
method_mvexp <- function() {

  function(data) {

    lambda_hat <- c(mean(data$x), mean(data$y))
    lambda_hat <- 1/lambda_hat

    function(x, y) {
      dexp(x, lambda_hat[1]) * dexp(y, lambda_hat[2])
    }

  }

}

# methods that return closures that compute a grid ------------------------

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

#' @export
method_histogram <- function(bins = NULL, smooth = "false", nudgex = "none", nudgey = "none") {

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

      # Sensible default for n near 1e4:
      # k <- 50

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

