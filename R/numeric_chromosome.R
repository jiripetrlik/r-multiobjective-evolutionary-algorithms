init_numeric_chromosome <- function(size) {
  return(runif(size))
}

simulated_binary_crossover <- function(chromosome1, chromosome2, nc = 2) {
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
  
  child1[child1 < 0] <- 0
  child1[child1 > 1] <- 1
  child2[child2 < 0] <- 0
  child2[child2 > 1] <- 1
  
  output <- list(child1 = child1, child2 = child2)
  return(output)
}

normally_distributed_mutation <- function(chromosome, sd = 0.01) {
  chromosome <- chromosome + rnorm(length(chromosome), sd = sd)
  chromosome[chromosome < 0] <- 0
  chromosome[chromosome > 1] <- 1
  
  return(chromosome)
}
