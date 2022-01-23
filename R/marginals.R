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
    warn("Groups with fewer than two data points have been dropped.")
    return(data.frame(
      x = NA_real_,
      density = NA_real_
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
