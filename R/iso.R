
kde_iso <- function(probs, data, nx, ny, rangex, rangey, h, adjust, type) {
  # The way n, h, and adjust are set up is consistent with stat_density_2d
  # Allows for easy tweaking of the MASS default
  if (is.null(h)) {
    h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
    h <- h * adjust
  }

  kdeout <- MASS::kde2d(
               x = data$x, y = data$y, n = c(nx, ny), h = h,
               lims = c(
                 scales::expand_range(rangex, .10),
                 scales::expand_range(rangey, .10)
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



mvnorm_iso <- function(probs, data, nx, ny, rangex, rangey, type) {

  data_matrix <- with(data, cbind(x, y))
  col_means <- colMeans(data_matrix)
  S <- cov(data_matrix)
  SInv <- solve(S)

  find_quantile <- function(x, mu, SigmaInv) {
    Mdist <- as.numeric(t(x - mu) %*% SigmaInv %*% (x - mu))

    pchisq(Mdist, df = 2)
  }

  df <- expand.grid(
    "x" = seq(rangex[1], rangex[2], length.out = nx),
    "y" = seq(rangey[1], rangey[2], length.out = ny)
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

  df <- df[c("x","y","fhat","fhat_discretized")]
  df$fhat <- rescale(df$fhat)

  breaks <- c(find_cutoff(df, probs), Inf)

  if (!smooth) {
    # Evaluate histogram on a grid
    # k*k points per histogram footprint
    k <- 50

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

    fhat <- split(df$fhat, factor(rep(1:ny, each = nx))) # split into rows
    fhat <- lapply(fhat, \(x) rep(x, each = k)) # repeat within rows (horizontal)
    fhat <- lapply(fhat, \(x) rep(x, times = k)) # repeat rows (vertical)
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


