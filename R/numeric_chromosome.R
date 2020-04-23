init_numeric_chromosome <- function(size) {
  return(runif(size))
}

init_numeric_chromosome_2 <- function(lower, upper) {
  return(runif(length(lower), lower, upper))
}

simulated_binary_crossover <- function(chromosome1, chromosome2, nc = 2, lower = c(), upper = c()) {
  if (length(chromosome1) != length(chromosome2)) {
    stop("Chromosomes must be of equal length")
  }
  size <- length(chromosome1)

  exponent <- 1 / (nc + 1)
  u <- runif(size)
  bqi <- rep(0, size)
  bqi[u <= 0.5] <- (2 * u[u <= 0.5]) ^ exponent
  bqi[u > 0.5] <- (1 / (2 * (1 - u[u > 0.5]))) ^ exponent

  child1 <- 0.5 * (((1 + bqi) * chromosome1) + ((1 - bqi) * chromosome2))
  child2 <- 0.5 * (((1 - bqi) * chromosome1) + ((1 + bqi) * chromosome2))

  if (length(lower) == 0) {
    child1[child1 < 0] <- 0
    child1[child1 > 1] <- 1
    child2[child2 < 0] <- 0
    child2[child2 > 1] <- 1
  } else {
    child1 <- pmax(child1, lower)
    child1 <- pmin(child1, upper)

    child2 <- pmax(child2, lower)
    child2 <- pmin(child2, upper)
  }

  output <- list(child1 = child1, child2 = child2)
  return(output)
}

normally_distributed_mutation <- function(chromosome, sd = 0.01, lower = c(), upper = c()) {
  chromosome <- chromosome + rnorm(length(chromosome), sd = sd)

  if (length(lower) == 0) {
    chromosome[chromosome < 0] <- 0
    chromosome[chromosome > 1] <- 1
  } else {
    chromosome <- pmax(chromosome, lower)
    chromosome <- pmin(chromosome, upper)
  }

  return(chromosome)
}

#' Scale values of numeric chromosome to the specified range
#'
#' Scale values of numeric chromosome to the specified range. Numeric chromosome
#' is a numeric vector with values within [0, 1].
#' @param chromosome Numeric chromosome
#' @param lower Minimal value(s) which item in chromosome can represent
#' @param upper Maximal value(s) which item in chromosome can represent
#' @return Numeric vector with converted chromosme values
#'
#' @examples
#' # Generate numeric chromosome with 10 items
#' chromosome <- runif(10)
#' # Scale values of numeric chromosome to [-5, 5]
#' scale_numeric_chromosome(chromosome, -5, 5)
#'
#' @export
scale_numeric_chromosome <- function(chromosome, lower = 0, upper = 1) {
  return(chromosome * (upper - lower) + lower)
}
