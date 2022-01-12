# this script contains several unexported helper functions



`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}



# unexported functions from ggplot2

xyz_to_isobands <- get("xyz_to_isobands", asNamespace("ggplot2"))
xyz_to_isolines <- get("xyz_to_isolines", asNamespace("ggplot2"))
iso_to_polygon <- get("iso_to_polygon", asNamespace("ggplot2"))
iso_to_path <- get("iso_to_path", asNamespace("ggplot2"))



# normalization/scaling functions
normalize <- function(v) v / sum(v)
standardize <- function(v, min = min(v), max = max(v)) (v - min) / (max - min)
rescale <- function(v) v / max(v)



# discrete approximation to volume integral
prob_above_c <- function(df, c) {
  if (length(c) > 1) return(vapply(c, prob_above_c, numeric(1)))
  sum(df$fhat_discretized[df$fhat >= c])
}


# Function computing sums for each height of the estimated density,
# completely specifying volume integrals given c of the Riemann surface
prob_integrals <- function(df) {
  c_vals <- unique(df$fhat)
  sums <- vapply(c_vals, function(x) sum(df$fhat_discretized[df$fhat > x]), numeric(1))
  df <- data.frame(c = c_vals, prob = sums)

  df
}

# numerical approximation for finding HDR
find_cutoff <- function(df, conf, uniroot = TRUE) {
  if (length(conf) > 1) return(vapply(conf, function(x) find_cutoff(df, x, uniroot), numeric(1)))

  # if method = "histogram", don't want to use uniroot,
  # runs into issue if n is small
  if (!uniroot) {
    # Find integrals for all important values of c
    integrals <- prob_integrals(df)

    # Smallest non-zero value of c, in case...
    cutoff_backup <- min(integrals$c[integrals$c != 0])

    # Filter to values of c which yield HDRs w/ integrals > conf
    diffs <- integrals$prob - conf
    integrals <- integrals[diffs >= 0,]

    # Find value of c with HDR integral closest to conf
    cutoff <- integrals$c[which.min(integrals$prob)]

    # Don't want to draw a contour at height 0
    # Slightly violates def of HDR, but produces much better graphics
    if (cutoff == 0) {
      return(cutoff_backup)
    }  else {
      return(cutoff)
    }
  }

  uniroot(function(c) prob_above_c(df, c) - conf, lower = 0, upper = 1)$root
}

