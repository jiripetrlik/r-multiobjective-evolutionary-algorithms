#' Vector Evaluated Genetic Algorithm
#'
#' Use Vector Evaluated Genetic Algorithm to solve the multiobjective optimization
#' problem.
#' @param objective_functions_list List of objective functions
#' @param chromosome_type Chromosome type ("binary" or "real-valued")
#' @param lower Lower bounds of the search space in case of real-valued GA
#' @param upper Upper bounds of the search space in case of real-valued GA
#' @param nBits Number of bits in binary chromosome
#' @param population_size Number of solutions evaluated in one iteration of genetic algorithm
#' @param number_of_iterations Number of iterations (generations) of genetic algorithm
#' @param nc NC for SBX crossover (valid if "numeric" chromosome is used)
#' @param mutation_probability Probability of mutation (valid if "binary" chromosome is used)
#' @param uniform_mutation_sd Standard deviation of mutation (valid if "numeric" chromosome is used)
#'
#' @return List which contains results of Vector Evaluated Genetic Algorithm:
#'
#' \code{values} - Matrix with objective functions values for nondominated solutions.
#' Each row represents one nondominated solution and each column one objective function.
#'
#' \code{nondominated_solutions} - Chromosomes of nondominated solutions
#'
#' \code{statistics} - Statistics about run of genetic algorithm
#'
#' \code{parameters} - Parameters of genetic algorithm
#'
#' @export
vega <- function(objective_functions_list,
                 chromosome_type,
                 lower = numeric(),
                 upper = numeric(),
                 nBits = 0,
                 population_size = length(objective_functions_list) * 40,
                 number_of_iterations = 100,
                 nc = 2,
                 mutation_probability = 0.05,
                 uniform_mutation_sd = 0.1) {
  if (chromosome_type == "binary") {
    binary_vega(objective_functions_list = objective_functions_list,
                nBits = nBits,
                population_size = population_size,
                number_of_iterations = number_of_iterations,
                mutation_probability = mutation_probability)
  }
  else if (chromosome_type == "real-valued") {
    numeric_vega(objective_functions_list = objective_functions_list,
                 lower = lower,
                 upper = upper,
                 population_size = population_size,
                 number_of_iterations = number_of_iterations,
                 nc = nc,
                 uniform_mutation_sd = uniform_mutation_sd)
  } else {
    stop("Unknown chromosome type")
  }
}

binary_vega <- function(objective_functions_list,
                        nBits,
                        population_size,
                        number_of_iterations,
                        mutation_probability = 0.05) {
  number_of_objective_functions <- length(objective_functions_list)
  subpopulation_size <- (population_size / number_of_objective_functions) * 2
  population <- replicate(population_size, init_binary_chromosome(nBits), simplify = FALSE)
  statistics <- list(min_fitness = list(), max_fitness = list(), mean_fitness = list(), sd_fitness = list())
  for (i in 1:number_of_objective_functions) {
    statistics$min_fitness[[i]] <- numeric()
    statistics$max_fitness[[i]] <- numeric()
    statistics$mean_fitness[[i]] <- numeric()
    statistics$sd_fitness[[i]] <- numeric()
  }

  for (iteration in 1:number_of_iterations) {
    # Create new candidate solutions
    for (i in 1:(population_size / 2)) {
      parents <- random_integer(1, population_size, 2)
      children <- one_point_crossover(population[[parents[1]]], population[[parents[2]]])
      population[[population_size + i * 2 - 1]] <- children$child1
      population[[population_size + i * 2]] <- children$child2
    }
    population[(population_size + 1):(2 * population_size)] <- lapply(population[(population_size + 1):(2 * population_size)],
                                                                      bind_parameters(
                                                                        binaryMutation, probability = mutation_probability))

    # Shuffle population
    population <- population[sample.int(2 * population_size)]

    # Evaluate objective functions
    objective_functions_values <- c()
    for (i in (1 : number_of_objective_functions)) {
      objective_functions_values <- c(objective_functions_values, sapply(population, objective_functions_list[[i]]))
    }
    objective_functions_values <- matrix(objective_functions_values, ncol = number_of_objective_functions)

    # Select new population
    selected <- c()
    for (i in 1:number_of_objective_functions) {
      subpopulation_fitness <- objective_functions_values[(((i - 1) * subpopulation_size) + 1) : (i * subpopulation_size), i]
      selected_subpopulation <- tournament_selection(subpopulation_fitness)
      selected_subpopulation <- selected_subpopulation + ((i - 1) * subpopulation_size)
      selected <- c(selected, selected_subpopulation[1 : (subpopulation_size / 2)])
    }
    population <- population[selected]
    objective_functions_values <- objective_functions_values[selected,]

    for (i in 1:number_of_objective_functions) {
      statistics$min_fitness[[i]] <- c(statistics$min_fitness[[i]], min(objective_functions_values[, i]))
      statistics$max_fitness[[i]] <- c(statistics$max_fitness[[i]], max(objective_functions_values[, i]))
      statistics$mean_fitness[[i]] <- c(statistics$mean_fitness[[i]], mean(objective_functions_values[, i]))
      statistics$sd_fitness[[i]] <- c(statistics$sd_fitness[[i]], sd(objective_functions_values[, i]))
    }
  }

  objective_functions_values <- c()
  for (i in (1 : number_of_objective_functions)) {
    objective_functions_values <- c(objective_functions_values, sapply(population, objective_functions_list[[i]]))
  }
  objective_functions_values <- matrix(objective_functions_values, ncol = number_of_objective_functions)
  nondominated <- find_nondominated(objective_functions_values)

  results <- list()
  results$values <- objective_functions_values[nondominated,]
  results$nondominated_solutions <- population[nondominated]
  results$statistics <- statistics

  parameters <- list()
  parameters$objective_functions_list <- objective_functions_list
  parameters$chromosome_type <- "binary"
  parameters$nBits <- nBits
  parameters$population_size <- population_size
  parameters$number_of_iterations <- number_of_iterations
  parameters$mutation_probability <- mutation_probability
  results$parameters <- parameters

  return(results)
}

numeric_vega <- function(objective_functions_list,
                         lower,
                         upper,
                         population_size,
                         number_of_iterations,
                         nc,
                         uniform_mutation_sd) {
  if (length(lower) != length(upper)) {
    stop("Size of lower and upper differ")
  }

  number_of_objective_functions <- length(objective_functions_list)
  subpopulation_size <- (population_size / number_of_objective_functions) * 2
  population <- replicate(population_size, init_numeric_chromosome(lower, upper), simplify = FALSE)
  statistics <- list(min_fitness = list(), max_fitness = list(), mean_fitness = list(), sd_fitness = list())
  for (i in 1:number_of_objective_functions) {
    statistics$min_fitness[[i]] <- numeric()
    statistics$max_fitness[[i]] <- numeric()
    statistics$mean_fitness[[i]] <- numeric()
    statistics$sd_fitness[[i]] <- numeric()
  }

  for (iteration in 1:number_of_iterations) {
    # Create new candidate solutions
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

    # Shuffle population
    population <- population[sample.int(2 * population_size)]

    # Evaluate objective functions
    objective_functions_values <- c()
    for (i in (1 : number_of_objective_functions)) {
      objective_functions_values <- c(objective_functions_values, sapply(population, objective_functions_list[[i]]))
    }
    objective_functions_values <- matrix(objective_functions_values, ncol = number_of_objective_functions)

    # Select new population
    selected <- c()
    for (i in 1:number_of_objective_functions) {
      subpopulation_fitness <- objective_functions_values[(((i - 1) * subpopulation_size) + 1) : (i * subpopulation_size), i]
      selected_subpopulation <- tournament_selection(subpopulation_fitness)
      selected_subpopulation <- selected_subpopulation + ((i - 1) * subpopulation_size)
      selected <- c(selected, selected_subpopulation[1 : (subpopulation_size / 2)])
    }
    population <- population[selected]
    objective_functions_values <- objective_functions_values[selected,]

    for (i in 1:number_of_objective_functions) {
      statistics$min_fitness[[i]] <- c(statistics$min_fitness[[i]], min(objective_functions_values[, i]))
      statistics$max_fitness[[i]] <- c(statistics$max_fitness[[i]], max(objective_functions_values[, i]))
      statistics$mean_fitness[[i]] <- c(statistics$mean_fitness[[i]], mean(objective_functions_values[, i]))
      statistics$sd_fitness[[i]] <- c(statistics$sd_fitness[[i]], sd(objective_functions_values[, i]))
    }
  }

  objective_functions_values <- c()
  for (i in (1 : number_of_objective_functions)) {
    objective_functions_values <- c(objective_functions_values, sapply(population, objective_functions_list[[i]]))
  }
  objective_functions_values <- matrix(objective_functions_values, ncol = number_of_objective_functions)
  nondominated <- find_nondominated(objective_functions_values)

  results <- list()
  results$values <- objective_functions_values[nondominated,]
  results$nondominated_solutions <- population[nondominated]
  results$statistics <- statistics

  parameters <- list()
  parameters$objective_functions_list <- objective_functions_list
  parameters$chromosome_type <- "real-valued"
  parameters$lower <- lower
  parameters$upper <- upper
  parameters$population_size <- population_size
  parameters$number_of_iterations <- number_of_iterations
  parameters$nc <- nc
  parameters$uniform_mutation_sd <- uniform_mutation_sd
  results$parameters <- parameters

  return(results)
}
