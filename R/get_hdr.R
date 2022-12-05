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
#' @param parameters An optional list of parameters governing the density estimation procedure.
#' @param HDR_membership Should HDR membership of data points be calculated?
#'   Defaults to `TRUE`, although it is computationally expensive.
#' @param fun Optional, a joint probability density function, must be vectorized in its first two arguments.
#' @param args Optional, a list of arguments to be provided to `fun`.
#'
#' @export
get_hdr <- function(method = "kde", data, probs = c(.99, .95, .8, .5), n = 100, rangex, rangey, parameters, HDR_membership = TRUE, fun, args = list()) {

  probs <- sort(probs, decreasing = TRUE)

  # Create df_est (estimated density evaluated on a grid) depending on specified method:
  if (is.character(method) && method == "fun") {

    df_est <- f_est(method = NULL, data = data, probs, n, rangex, rangey, fun = fun, args = args)

  } else if (is.character(method)) {

    df_est <- switch(method,
      "kde" = kde_est(data, probs, n, rangex, rangey, parameters),
      "histogram" = histogram_est(data, probs, rangex, rangey, parameters),
      "freqpoly" = freqpoly_est(data, probs, rangex, rangey, parameters),
      "mvnorm" = f_est(method_mvnorm, data, probs, n, rangex, rangey, parameters)
    )

    if (!method %in% c("kde", "mvnorm", "histogram", "freqpoly")) stop("Invalid method specified")

  } else if (is.function(method)) {

    df_est <- f_est(method, data, probs, n, rangex, rangey, parameters)

  }

  # Manipulate df_est to get information about HDRs:

  # force estimate to integrate to 1
  df_est$fhat_discretized <- normalize(df_est$fhat)

  # temporarily rescale df$fhat for stability
  fhat_max <- max(df_est$fhat)
  df_est$fhat <- df_est$fhat / fhat_max

  # find cutoffs (in terms of rescaled fhat)
  breaks <- c(find_cutoff(df_est, probs), Inf)

  # find hdr membership of points from data
  if (!is.null(data) & HDR_membership) {
    data$HDR_membership <- mapply(get_HDR_membership, data$x, data$y, MoreArgs = list(df_est, breaks, probs), SIMPLIFY = TRUE)
  }

  # transforming df_est$fhat and breaks back to original scale:
  df_est$fhat <- df_est$fhat * fhat_max
  breaks <- breaks * fhat_max

  # bundle everything together
  list(
    df_est = df_est,
    breaks = breaks,
    data = data
  )

}

get_HDR_membership <- function(x, y, df_est, breaks, probs) {
  df_est$dist <- (x - df_est$x)^2 + (y - df_est$y)^2
  fhat <- df_est[which.min(df_est$dist), "fhat"]

  HDRs <- which(fhat >= breaks)
  if (length(HDRs) == 0) return(1)
  probs[max(HDRs)]
}


kde_est <- function(data, probs, n, rangex, rangey, parameters) {

  if (is.null(parameters$adjust)) parameters$adjust <- 1

  if (is.null(parameters$h)) {
    parameters$h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
    parameters$h <- parameters$h * parameters$adjust
  }

  kdeout <- MASS::kde2d(
    x = data$x, y = data$y, n = n, h = parameters$h,
    lims = c(rangex, rangey)
  )

  df <- with(kdeout, expand.grid("x" = x, "y" = y))
  df$fhat <- as.vector(kdeout$z)

  df

}


histogram_est <- function(data, probs, rangex, rangey, parameters) {

  if (is.null(parameters$smooth)) parameters$smooth <- FALSE
  if (is.null(parameters$nudgex)) parameters$nudgex <- "none"
  if (is.null(parameters$nudgey)) parameters$nudgey <- "none"

  bins <- parameters$bins

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


  if (parameters$smooth) {

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


freqpoly_est <- function(data, probs, rangex, rangey, parameters) {

  bins <- parameters$bins

  if (is.null(bins)) {
    bins <- numeric(2)

    # define histogram mesh according to Scott p. 87
    # To-Do: fill in with rules for frequency polygons
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


# method is a function of data
# fun is a function of vectors x, y
# Might need to be more careful w/ axis transformations here
f_est <- function(method, data, probs, n, rangex, rangey, parameters = list(), fun = NULL, args = list()) {

  # If fun isn't specified, method returns a closure
  # representing closed form of density estimate
  fun <- fun %||% method(data, parameters)

  # grid to evaluate fun
  df <- expand.grid(
    "x" = seq(rangex[1], rangex[2], length.out = n),
    "y" = seq(rangey[1], rangey[2], length.out = n)
  )

  # evaluate method on the grid, f required to be vectorized in x, y:
  df$fhat <- do.call(fun, c(quote(df$x), quote(df$y), args))

  df

}

# TODO - document these, along with the *_est functions from above

#' @export
method_mvnorm <- function(data, parameters) {
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

#' @export
method_mvexp <- function(data, parameters) {
  lambda_hat <- c(mean(data$x), mean(data$y))
  lambda_hat <- 1/lambda_hat

  function(x, y) {
    dexp(x, lambda_hat[1]) * dexp(y, lambda_hat[2])
  }
}

