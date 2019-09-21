init_binary_chromosome <- function(size) {
  return(sample(c(TRUE, FALSE), size, TRUE))
}

one_point_crossover <- function(chromosome1, chromosome2) {
  if (length(chromosome1) != length(chromosome2)) {
    stop("Chromosomes must be of equal length")
  }
  size <- length(chromosome1)

  point <- random_integer(1, size - 1)
  child1 <- c(chromosome1[1:point], chromosome2[(point + 1):size])
  child2 <- c(chromosome2[1:point], chromosome1[(point + 1):size])

  output <- list(child1 = child1, child2 = child2)
  return(output)
}

#' @importFrom stats runif
uniform_crossover <- function(chromosome1, chromosome2) {
  if (length(chromosome1) != length(chromosome2)) {
    stop("Chromosomes must be of equal length")
  }
  size <- length(chromosome1)

  mask <- runif(size) > 0.5
  child1 <- chromosome1
  child1[mask] <- chromosome2[mask]
  child2 <- chromosome2
  child2[!mask] <- chromosome1[!mask]

  output <- list(child1 = child1, child2 = child2)
  return(output)
}

#' @importFrom stats runif
binaryMutation <- function(parent, probability = 0.05) {
  mask <- runif(length(parent)) < probability
  child <- parent
  child[mask] <- (!parent)[mask]

  return(child)
}

#' Convert binary chromosome to numeric value
#'
#' Convert binary chromosome to numeric value.
#' @param chromosome Binary chromosome
#' @param min Minimal value which chromosome can represent
#' @param max Maximal value which chromosome can represent
#' @return Numeric value within the range specified by \code{min}, \code{max}
#'
#' @examples
#' # Generate a random 24 bit chromosome
#' chromosome <- sample(c(TRUE, FALSE), 24, TRUE)
#' Convert the chromosome to a numeric value within the range [-10, 10]
#' binary_chromosome_to_numeric(chromosome, -10, 10)
#'
#' @export
binary_chromosome_to_numeric <- function(chromosome, min = 0, max = 1) {
  int_value <- sum(as.integer(chromosome) * 2 ^ seq(0, length(chromosome) -1))
  max_int_value <- 2 ^ length(chromosome) - 1
  return((int_value / max_int_value) * (max - min) + min)
}
