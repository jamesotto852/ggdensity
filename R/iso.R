
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

  df <- expand.grid(x = kdeout$x, y = kdeout$y)

  df$fhat <- as.vector(kdeout$z)
  df$fhat_discretized <- normalize(df$fhat)
  df$fhat <- rescale(df$fhat)

  breaks <- c(find_cutoff(df, probs), Inf)

  df <- data.frame(
    x = df$x,
    y = df$y,
    z = df$fhat
  )

  if (type == "bands") {
    xyz_to_isobands(df, breaks)
  } else {
    xyz_to_isolines(df, breaks)
  }
}

mvnorm_iso <- function(probs, data, nx, ny, rangex, rangey, type) {
  data_matrix <- matrix(c(data$x, data$y), ncol = 2)
  S <- cov(data_matrix)
  M <- apply(data_matrix, 2, mean)
  SInv <- solve(S)

  find_quantile <- function(x, mu, SigmaInv) {
    Mdist <- as.numeric(t(x - mu) %*% SigmaInv %*% (x - mu))

    pchisq(Mdist, 2)
  }

  df <- expand.grid(x = seq(rangex[1], rangex[2], length.out = nx),
                    y = seq(rangey[1], rangey[2], length.out = ny))


  df$z <- apply(df, 1, \(.) find_quantile(., M, SInv))

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


histogram_iso <- function(probs, df, nx, ny, rangex, rangey, nudgex, nudgey, type) {
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


  df_cuts <- data.frame(
    xbin = cut(xvals, sx),
    ybin = cut(yvals, sy)
  )


  df <- expand.grid(xbin = levels(df_cuts$xbin), ybin = levels(df_cuts$ybin))
  df$n <- as.vector(table(df_cuts$xbin, df_cuts$ybin))

  df$xbin_midpt <- xbin_mdpts[as.integer(df$xbin)]
  df$ybin_midpt <- ybin_mdpts[as.integer(df$ybin)]

  df$xmin <- df$xbin_midpt - de_x/2
  df$xmax <- df$xbin_midpt + de_x/2
  df$de_x <- de_x

  df$ymin <- df$ybin_midpt - de_y/2
  df$ymax <- df$ybin_midpt + de_y/2
  df$de_y <- de_y

  df$fhat <- df$n / (sum(df$n) * box_area)
  df$fhat_discretized <- normalize(df$fhat)


  if(nudgex == "left") df$x <- df$xmin
  if(nudgex == "none") df$x <- df$xbin_midpt
  if(nudgex == "right") df$x <- df$xmax

  if(nudgey == "down") df$y <- df$ymin
  if(nudgey == "none") df$y <- df$ybin_midpt
  if(nudgey == "up") df$y <- df$ymax

  df <- df[c("x","y","fhat","fhat_discretized")]
  df$fhat <- rescale(df$fhat)

  breaks <- c(find_cutoff(df, probs), Inf)

  df <- df[c("x","y","fhat")]
  names(df) <- c("x","y","z")

  if (type == "bands") {
    xyz_to_isobands(df, breaks)
  } else {
    xyz_to_isolines(df, breaks)
  }
}


