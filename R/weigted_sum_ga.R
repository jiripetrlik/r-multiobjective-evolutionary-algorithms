weighted_sum_function <- function(chromosome, functions, weights) {
  values <- sapply(functions, function(f) { f(chromosome) })
  return(sum(weights * values))
}

#' Weighted sum genetic algorithm
#'
#' Use weighted sum approach to solve multiobjective optimization problem.
#' Weighted sum approach transforms multiobjective optimization problem
#' to single objective by multiplying objective functions values by weights.
#' Then genetic algorithm is used to find optimal solution.
#' @param objective_functions_list List of objective functions
#' @param weights Objective functions weights
#' @param chromosome_type Chromosome type ("binary" or "real-valued")
#' @param lower Lower bounds of the search space in case of real-valued GA
#' @param upper Upper bounds of the search space in case of real-valued GA
#' @param nBits Number of bits in binary chromosome
#' @param population_size Number of solutions evaluated in one iteration of genetic algorithm
#' @param number_of_iterations Number of iterations (generations) of genetic algorithm
#' @param elitism Use elitism
#' @param nc NC for SBX crossover (valid if "numeric" chromosome is used)
#' @param mutation_probability Probability of mutation (valid if "binary" chromosome is used)
#' @param uniform_mutation_sd Standard deviation of mutation (valid if "numeric" chromosome is used)
#'
#' @return List which contains results of weighted sum genetic algorithm:
#'
#' \code{value} - Sum of weighted objective funtions values for the best solution
#'
#' \code{best_solution} - Chromosome which represents the best solution
#'
#' \code{best_solution_index} - Index of the best solution in population
#'
#' \code{statistics} - Statistics about run of genetic algorithm
#'
#' \code{parameters} - Parameters of genetic algorithm
#'
#' \code{values} - Values of objective functions for the best solution
#'
#' \code{weighted_values} - Values of objective functions multiplied by
#' weights for the best solution
#'
#' @export
weighted_sum_ga <- function(objective_functions_list,
                                weights,
                                chromosome_type,
                                lower = numeric(),
                                upper = numeric(),
                                nBits = 0,
                                chromosome_size,
                                population_size = 100,
                                number_of_iterations = 100,
                                elitism = TRUE,
                                nc = 2,
                                mutation_probability = 0.05,
                                uniform_mutation_sd = 0.01) {
  objective_function <- bind_parameters(weighted_sum_function, functions = objective_functions_list, weights = weights)
  results <- single_objective_ga(objective_function,
                                chromosome_type,
                                lower,
                                upper,
                                nBits,
                                population_size,
                                number_of_iterations,
                                elitism,
                                nc,
                                mutation_probability,
                                uniform_mutation_sd)

  results$values <- sapply(objective_functions_list, function(f) {
      scale_numeric_chromosome(results$best_solution, lower, upper)
      f(results$best_solution)
    })
  results$weighted_values <- results$values * weights

  results$parameters$objective_functions_list <- objective_functions_list
  results$parameters$weights <- weights

  return(results)
}
