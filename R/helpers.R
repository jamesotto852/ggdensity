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



# discrete approximation to P[f^(X,Y) >= c]
prob_above_c <- function(df, c) {
  if (length(c) > 1) return(vapply(c, prob_above_c, df = df, numeric(1)))
  with(df, sum(fhat_discretized[fhat >= c]))
}



# numerical approximation for finding hdr
# if method = "histogram", don't want to use uniroot, runs into issue if n is small
find_cutoff <- function(df, conf, collapse = FALSE, uniroot = TRUE) {

  if (length(conf) > 1) return(vapply(conf, function(x) find_cutoff(df, x, collapse, uniroot), numeric(1)))

  # the following is set to FALSE to override when other code calls
  # the function with uniroot = TRUE, remove once we're confident the
  # code works in all cirumstances
  if (FALSE) {

    uniroot(function(c) prob_above_c(df, c) - conf, lower = 0, upper = 1)$root

  } else {

    # sort df rows by fhat
    df <- df[order(df$fhat, decreasing = TRUE),]

    if (collapse) {

      # collapse rows of df s.t. each contains a unique value of fhat_discretized
      df_collapsed <- data.frame(fhat = unique(df$fhat))
      df_collapsed$fhat_discretized <- vapply(df_collapsed$fhat, function(x) sum(df$fhat_discretized[df$fhat == x]), numeric(1))

      df <- df_collapsed

    }

    # compute cumsum of probs
    df$cumprob <- cumsum(df$fhat_discretized)

    # determine cutoff
    max(df$fhat[df$cumprob >= conf])
    # Potential issue: the maximum can be 0 if method = "histogram"

  }

}

