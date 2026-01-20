.onAttach <- function(...) {
  random_digit <- function() {
    time <- as.character(Sys.time())
    digit <- substr(time, nchar(time), nchar(time))
    as.integer(digit)
  }
  if(!interactive() || random_digit() > 0.1) return()
  packageStartupMessage('  Please cite ggdensity! See citation("ggdensity") for details.')
}
