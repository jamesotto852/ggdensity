# this script contains several unexported helper functions

# normalization/scaling functions
normalize <- function(v) v / sum(v)

# numerical approximation for finding hdr
# if method = "histogram", don't want to use uniroot, runs into issue if n is small
find_cutoff <- function(df, conf, uniroot = TRUE) {

  if (length(conf) > 1) return(vapply(conf, function(x) find_cutoff(df, x, uniroot), numeric(1)))

  # sort df rows by fhat
  df <- df[order(df$fhat, decreasing = TRUE),]

  # compute cumsum of probs
  df$cumprob <- cumsum(df$fhat_discretized)

  # determine cutoff
  max(df[df$cumprob >= conf,]$fhat)

}
