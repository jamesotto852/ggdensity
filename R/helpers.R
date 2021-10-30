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



# numerical approximation for finding HDR
find_cutoff <- function(df, conf) {
  if (length(conf) > 1) return(vapply(conf, function(x) find_cutoff(df, x), numeric(1)))
  uniroot(function(c) prob_above_c(df, c) - conf, lower = 0, upper = 1)$root
}
