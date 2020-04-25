#' Single objective genetic algorithm
#'
#' Use single objective genetic algorithm to find an optimum for
#' the specified objective function. Candidate solutions are represented
#' as logical or numeric vectors.
#' @param objective_function Objective function
#' @param chromosome_type Chromosome type ("binary" or "real-valued")
#' @param lower Lower bounds of the search space in case of real-valued GA
#' @param upper Upper bounds of the search space in case of real-valued GA
#' @param nBits Number of bits in binary chromosome
#' @param population_size Number of solutions evaluated in one iteration of genetic algorithm
#' @param number_of_iterations Number of iterations (generations) of genetic algorithm
#' @param elitism Use elitism
#' @param nc NC for SBX crossover (valid if "real-valued" chromosome is used)
#' @param mutation_probability Probability of mutation (valid if "binary" chromosome is used)
#' @param uniform_mutation_sd Standard deviation of mutation (valid if "real-valued" chromosome is used)
#'
#' @return List which contains results of single objective genetic algorithm:
#'
#' \code{value} - Value of objective function for the best solution
#'
#' \code{best_solution} - Chromosome which represents the best solution
#'
#' \code{best_solution_index} - Index of the best solution in population
#'
#' \code{statistics} - Statistics about run of genetic algorithm
#'
#' \code{parameters} - Parameters of genetic algorithm
#'
#' @export
single_objective_ga <- function(objective_function,
                                chromosome_type,
                                lower = numeric(),
                                upper = numeric(),
                                nBits = 0,
                                population_size = 100,
                                number_of_iterations = 100,
                                elitism = TRUE,
                                nc = 2,
                                mutation_probability = 0.05,
                                uniform_mutation_sd = 0.1) {
  if (chromosome_type == "binary") {
    binary_single_objective_ga(objective_function, nBits, population_size,
                               number_of_iterations, elitism, mutation_probability)
  } else if (chromosome_type == "real-valued") {
    real_valued_single_objective_ga(objective_function, lower, upper, population_size,
                                    number_of_iterations, elitism, nc, uniform_mutation_sd)
  } else {
    stop("Unknown chromosome type")
  }
}

binary_single_objective_ga <- function(objective_function,
                                       nBits,
                                       population_size,
                                       number_of_iterations,
                                       elitism,
                                       mutation_probability) {
  population <- replicate(population_size, init_binary_chromosome(nBits), simplify = FALSE)
  statistics <- list(min_fitness = numeric(), max_fitness = numeric(), mean_fitness = numeric(), sd_fitness = numeric())
  for (iteration in 1:number_of_iterations) {
    for (i in 1:(population_size / 2)) {
      parents <- random_integer(1, population_size, 2)
      children <- one_point_crossover(population[[parents[1]]], population[[parents[2]]])
      population[[population_size + i * 2 - 1]] <- children$child1
      population[[population_size + i * 2]] <- children$child2
    }
    population[(population_size + 1):(2 * population_size)] <- lapply(population[(population_size + 1):(2 * population_size)],
                                                                      bind_parameters(
                                                                        binaryMutation, probability = mutation_probability))
    fitness_values <- sapply(population, objective_function)
    selected <- tournament_selection(fitness_values)
    if (elitism == TRUE) {
      selected[1] <- which.min(fitness_values)
    }
    population <- population[selected]
    fitness_values <-fitness_values[selected]

    statistics$min_fitness <- c(statistics$min_fitness, min(fitness_values))
    statistics$max_fitness <- c(statistics$max_fitness, max(fitness_values))
    statistics$mean_fitness <- c(statistics$mean_fitness, mean(fitness_values))
    statistics$sd_fitness <- c(statistics$sd_fitness, sd(fitness_values))
  }
  best_solution_index <- which.min(fitness_values)

  results <- list()
  results$value <- fitness_values[best_solution_index]
  results$best_solution <- population[[best_solution_index]]
  results$best_solution_index <- best_solution_index
  results$statistics <- statistics

  parameters <- list()
  parameters$objective_function <- objective_function
  parameters$nBits <- nBits
  parameters$chromosome_type <- "binary"
  parameters$population_size <- population_size
  parameters$number_of_iterations <- number_of_iterations
  parameters$elitism <- elitism
  parameters$mutation_probability <- mutation_probability
  results$parameters <- parameters

  return(results)
}

real_valued_single_objective_ga <- function(objective_function,
                                        lower,
                                        upper,
                                        population_size,
                                        number_of_iterations,
                                        elitism,
                                        nc,
                                        uniform_mutation_sd) {
  if (length(lower) != length(upper)) {
    stop("Size of lower and upper differ")
  }
  chromosome_size <- length(lower)

  population <- replicate(population_size, init_numeric_chromosome_2(lower, upper), simplify = FALSE)
  statistics <- list(min_fitness = numeric(), max_fitness = numeric(), mean_fitness = numeric(), sd_fitness = numeric())
  for (iteration in 1:number_of_iterations) {
    for (i in 1:(population_size / 2)) {
      parents <- random_integer(1, population_size, 2)
      children <- simulated_binary_crossover(population[[parents[1]]], population[[parents[2]]], nc, lower, upper)
      population[[population_size + i * 2 - 1]] <- children$child1
      population[[population_size + i * 2]] <- children$child2
    }
    population[(population_size + 1):(2 * population_size)] <- lapply(population[(population_size + 1):(2 * population_size)],
                                                                      bind_parameters(
                                                                        normally_distributed_mutation, sd = uniform_mutation_sd,
                                                                        lower = lower, upper = upper))
    fitness_values <- sapply(population, objective_function)
    selected <- tournament_selection(fitness_values)
    if (elitism == TRUE) {
      selected[1] <- which.min(fitness_values)
    }
    population <- population[selected]
    fitness_values <-fitness_values[selected]

    statistics$min_fitness <- c(statistics$min_fitness, min(fitness_values))
    statistics$max_fitness <- c(statistics$max_fitness, max(fitness_values))
    statistics$mean_fitness <- c(statistics$mean_fitness, mean(fitness_values))
    statistics$sd_fitness <- c(statistics$sd_fitness, sd(fitness_values))
  }
  best_solution_index <- which.min(fitness_values)

  results <- list()
  results$value <- fitness_values[best_solution_index]
  results$best_solution <- population[[best_solution_index]]
  results$best_solution_index <- best_solution_index
  results$statistics <- statistics

  parameters <- list()
  parameters$objective_function <- objective_function
  parameters$lower <- lower
  parameters$upper <- upper
  parameters$chromosome_type <- "real-valued"
  parameters$population_size <- population_size
  parameters$number_of_iterations <-number_of_iterations
  parameters$elitism <- elitism
  parameters$nc <- nc
  parameters$uniform_mutation_sd <- uniform_mutation_sd
  results$parameters <- parameters

  return(results)
}
