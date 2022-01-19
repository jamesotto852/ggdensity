.onAttach <- function(...) {
  if(!interactive() || stats::runif(1) > 0.1) return()
  packageStartupMessage('  Please cite ggdensity! See citation("ggdensity") for details.')
}
