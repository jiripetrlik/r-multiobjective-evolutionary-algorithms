bind_parameters <- function(f, ...) {
  parameters <- list(...)
  formals(f)[names(parameters)] <- parameters
  return(f)
}

#' @importFrom stats runif
random_integer <- function(min, max, n = 1) {
  min <- floor(min)
  max <- ceiling(max + 1)
  as.integer(floor(runif(n, min, max)))
}
