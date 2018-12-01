#' @importFrom stats runif
random_integer <- function(min, max, n = 1) {
  min <- floor(min)
  max <- ceiling(max + 1)
  floor(runif(n, min, max))
}
