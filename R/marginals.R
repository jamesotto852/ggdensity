# Function factory for assigning HDR membership:
# (Maybe belongs in helpers.R)
assign_cutoff <- function(probs, cutoffs) {

  # ensure ordering is correct
  probs <- probs[order(probs, decreasing = TRUE)]
  cutoffs <- cutoffs[order(cutoffs, decreasing = FALSE)]

  f <- function(fhat) {
    if (length(fhat) > 1) return(vapply(fhat, f, numeric(1)))
    if (fhat < min(cutoffs)) return(NA_real_)

    probs[max(which(fhat >= cutoffs))]
  }

  f
}


# Inspired by ggplot2::compute_density()
kde_marginal <- function(x, w, from, to, bw = "nrd0", adjust = 1,
                            kernel = "gaussian", n = 512) {
  nx <- length(x)
  if (is.null(w)) {
    w <- rep(1 / nx, nx)
  } else {
    w <- w / sum(w)
  }

  # if less than 2 points return data frame of NAs and a warning
  if (nx < 2) {
    message("Groups with fewer than two data points have been dropped.")
    return(data.frame(
      x = NA_real_,
      fhat = NA_real_,
      fhat_discretized = NA_real_
    ))
  }

  dens <- stats::density(x, weights = w, bw = bw, adjust = adjust,
                         kernel = kernel, n = n, from = from, to = to)

  data.frame(
    x = dens$x,
    fhat = dens$y,
    fhat_discretized = normalize(dens$y)
  )
}


hist_marginal <- function(x, from, to, bins) {
  nx <- length(x)

  # Default to normal reference rule (Scott p. 59)
  if (is.null(bins)) {
    hx <- 3.504 * sd(x) * nx^(-1/3)
    bins <- round((to - from) / hx)
  }

  # if less than 2 points return data frame of NAs and a warning
  if (nx < 2) {
    message("Groups with fewer than two data points have been dropped.")
    return(data.frame(
      x = NA_real_,
      fhat = NA_real_,
      fhat_discretized = NA_real_
    ))
  }

  sx <- seq(from, to, length.out = bins + 1)
  de_x <- sx[2] - sx[1]
  midpts <- sx[-(bins+1)] + de_x/2
  n <- as.numeric(table(cut(x, sx)))

  data.frame(
    x = midpts,
    fhat = normalize(n),
    fhat_discretized = normalize(n)
  )
}

freqpoly_marginal <- function(x, from, to, bins, n = 512) {
  df <- hist_marginal(x, from, to, bins)
  hx <- df$x[2] - df$x[1]

  # need to pad df from hist_marginal() w/ bins that have est prob of 0
  # so that we can interpolate
  df <- rbind(
    # add initial bin w/ est prob of 0
    data.frame(
      x = min(df$x) - hx,
      fhat = 0,
      fhat_discretized = 0
    ),

    # include original histogram estimator
    df,

    # add final bin w/ est prob of 0
    data.frame(
      x = max(df$x) + hx,
      fhat = 0,
      fhat_discretized = 0
    )
  )

  sx <- seq(from, to, length.out = n)

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
    fhat = dens,
    fhat_discretized = normalize(dens)
  )

}


norm_marginal <- function(x, from, to, n = 512) {
  nx <- length(x)

  # if less than 2 points return data frame of NAs and a warning
  if (nx < 2) {
    message("Groups with fewer than two data points have been dropped.")
    return(data.frame(
      x = NA_real_,
      fhat = NA_real_,
      fhat_discretized = NA_real_
    ))
  }

  sx <- seq(from, to, length.out = n)

  mu_x <- mean(x)
  sd_x <- sd(x)
  dens <- dnorm(sx, mu_x, sd_x)

  data.frame(
    x = sx,
    fhat = dens,
    fhat_discretized = normalize(dens)
  )
}
