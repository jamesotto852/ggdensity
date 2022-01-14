
kde_iso <- function(probs, data, res, rangex, rangey, h, adjust, type) {
  # The way n, h, and adjust are set up is consistent with stat_density_2d
  # Allows for easy tweaking of the MASS default
  if (is.null(h)) {
    h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
    h <- h * adjust
  }

  kdeout <- MASS::kde2d(
               x = data$x, y = data$y, n = res, h = h,
               lims = c(
                 scales::expand_range(rangex, .25),
                 scales::expand_range(rangey, .25)
               )
             )

  df <- with(kdeout, expand.grid("x" = x, "y" = y))

  df$fhat <- as.vector(kdeout$z)
  df$fhat_discretized <- normalize(df$fhat)
  df$fhat <- rescale(df$fhat)

  breaks <- c(find_cutoff(df, probs), Inf)

  df <- with(df, data.frame("x" = x, "y" = y, "z" = fhat))

  if (type == "bands") {
    xyz_to_isobands(df, breaks)
  } else {
    xyz_to_isolines(df, breaks)
  }
}



mvnorm_iso <- function(probs, data, res, rangex, rangey, type) {

  data_matrix <- with(data, cbind(x, y))
  col_means <- colMeans(data_matrix)
  S <- cov(data_matrix)
  SInv <- solve(S)

  find_quantile <- function(x, mu, SigmaInv) {
    Mdist <- as.numeric(t(x - mu) %*% SigmaInv %*% (x - mu))

    pchisq(Mdist, df = 2)
  }

  rangex <- scales::expand_range(rangex, .25)
  rangey <- scales::expand_range(rangey, .25)

  df <- expand.grid(
    "x" = seq(rangex[1], rangex[2], length.out = res),
    "y" = seq(rangey[1], rangey[2], length.out = res)
  )

  df$z <- apply(df, 1, find_quantile, mu = col_means, SigmaInv = SInv)

  if (min(probs) == 0) {
    breaks <- probs
  } else {
    breaks <- c(probs, 0)
  }

  if (type == "bands") {
    xyz_to_isobands(df, breaks)
  } else {
    xyz_to_isolines(df, breaks)
  }

}


histogram_iso <- function(probs, df, nx, ny, rangex, rangey, nudgex, nudgey, smooth, type) {
  xvals <- df$x
  yvals <- df$y

  xbtwn <- (rangex[1] <= xvals & xvals <= rangex[2])
  if (!all(xbtwn)) {
    # warning("xlim does not contain range of x values.", call. = FALSE)
    xvals <- xvals[xbtwn]
    yvals <- yvals[xbtwn]
  }

  ybtwn <- (rangey[1] <= yvals & yvals <= rangey[2])
  if (!all(ybtwn)) {
    # warning("ylim does not contain range of y values.", call. = FALSE)
    xvals <- xvals[ybtwn]
    yvals <- yvals[ybtwn]
  }


  sx <- seq(rangex[1], rangex[2], length.out = nx + 1)
  sy <- seq(rangey[1], rangey[2], length.out = ny + 1)
  de_x <- sx[2] - sx[1]
  de_y <- sy[2] - sy[1]
  box_area <- de_x * de_y

  xbin_mdpts <- sx[-(nx+1)] + de_x/2
  ybin_mdpts <- sy[-(ny+1)] + de_y/2

  xleft <- sx[-(nx+1)]
  xright <- sx[-1]

  ybottom <- sy[-(ny+1)]
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


  if (smooth) {
    if(nudgex == "left") df$x <- df$xmin
    if(nudgex == "none") df$x <- df$xbin_midpt
    if(nudgex == "right") df$x <- df$xmax

    if(nudgey == "down") df$y <- df$ymin
    if(nudgey == "none") df$y <- df$ybin_midpt
    if(nudgey == "up") df$y <- df$ymax
  } else {
    # No nudging if we're not smoothing
    df$x <- df$xbin_midpt
    df$y <- df$ybin_midpt
  }

  n <- sum(df$n)
  df <- df[c("x","y","fhat","fhat_discretized")]
  df$fhat <- rescale(df$fhat)

  breaks <- c(find_cutoff(df, probs, uniroot = FALSE), Inf)

  if (!smooth) {
    # Evaluate histogram on a grid
    # For xyz_to_iso* funs, need tightly packed values for good isobands/lines
    # k*k points per histogram footprint
    # Higher values of k -> better visuals, more computationally expensive

    # Sensible default for n near 1e4:
    # k <- 50

    # The "optimal" value of k seems to be on the order of O(n^(-1/3))
    # found constant which yields k = 50 for n = 1000
    if (n > 1000) {
      k <- floor(500/(n^(1/3)))
    } else{
      k <- 50
    }

    nnx <- nx * k
    nny <- ny * k

    ssx <- seq(rangex[1], rangex[2], length.out = nnx)
    ssy <- seq(rangey[1], rangey[2], length.out = nny)

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


    # m <- matrix(df$fhat, nrow = ny, byrow = TRUE)
    # ddf$fhat <- as.vector(kronecker(m, matrix(1, k, k)))


    fhat <- split(df$fhat, factor(rep(1:ny, each = nx))) # split into rows
    fhat <- lapply(fhat, function(x) rep(x, each = k)) # repeat within rows (horizontal)
    fhat <- lapply(fhat, function(x) rep(x, times = k)) # repeat rows (vertical)
    fhat <- unlist(fhat) # concatenate
    ddf$fhat <- fhat

    df <- ddf
  }

  df <- setNames(df[c("x","y","fhat")], c("x","y","z"))


  if (type == "bands") {
    xyz_to_isobands(df, breaks)
  } else {
    xyz_to_isolines(df, breaks)
  }
}


freqpoly_iso <- function(probs, df, nx, ny, rangex, rangey, type) {
  xvals <- df$x
  yvals <- df$y

  xbtwn <- (rangex[1] <= xvals & xvals <= rangex[2])
  if (!all(xbtwn)) {
    # warning("xlim does not contain range of x values.", call. = FALSE)
    xvals <- xvals[xbtwn]
    yvals <- yvals[xbtwn]
  }

  ybtwn <- (rangey[1] <= yvals & yvals <= rangey[2])
  if (!all(ybtwn)) {
    # warning("ylim does not contain range of y values.", call. = FALSE)
    xvals <- xvals[ybtwn]
    yvals <- yvals[ybtwn]
  }


  de_x <- (rangex[2] - rangex[1]) / nx
  de_y <- (rangey[2] - rangey[1]) / ny
  rangex[1] <- rangex[1] - de_x
  rangex[2] <- rangex[2] + de_x
  rangey[1] <- rangey[1] - de_y
  rangey[2] <- rangey[2] + de_y
  nx <- nx + 2
  ny <- ny + 2
  sx <- seq(rangex[1], rangex[2], length.out = nx + 1)
  sy <- seq(rangey[1], rangey[2], length.out = ny + 1)


  box_area <- de_x * de_y

  xbin_mdpts <- sx[-(nx+1)] + de_x/2
  ybin_mdpts <- sy[-(ny+1)] + de_y/2

  xleft <- sx[-(nx+1)]
  xright <- sx[-1]

  ybottom <- sy[-(ny+1)]
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
    x = sx[2:nx],
    y = sy[2:ny]
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


  # Here, the necessary value of k seems to be O(n(1/2))
  # Found coefficient by setting k(n=1000) = 25
  n <- sum(df$n)
  k <- if (n > 1000) floor(790/(n^(1/2))) else 25

  surface_list <- apply(df_A, 1, coeffs_to_surface, k, simplify = FALSE)
  df <- do.call(rbind, surface_list)

  df$fhat_discretized <- normalize(df$fhat)
  df$fhat <- rescale(df$fhat)

  breaks <- c(find_cutoff(df, probs), Inf)

  df <- setNames(df[c("x","y","fhat")], c("x","y","z"))

  if (type == "bands") {
    xyz_to_isobands(df, breaks)
  } else {
    xyz_to_isolines(df, breaks)
  }
}


