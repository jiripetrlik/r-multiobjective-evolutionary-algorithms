weighted_sum_function <- function(chromosome, functions, weights) {
  values <- sapply(functions, function(f) { f(chromosome) })
  return(sum(weights * values))
}

#' @export
weighted_sum_ga <- function(objective_functions_list,
                                weights,
                                chromosome_size,
                                chromosome_type = "binary",
                                population_size = 100,
                                number_of_iterations = 100,
                                elitism = TRUE,
                                nc = 2,
                                mutation_probability = 0.05,
                                uniform_mutation_sd = 0.01) {
  objective_function <- bind_parameters(weighted_sum_function, functions = objective_functions_list, weights = weights)
  results <- single_objective_ga(objective_function,
                                chromosome_size,
                                chromosome_type,
                                population_size,
                                number_of_iterations,
                                elitism,
                                nc,
                                mutation_probability,
                                uniform_mutation_sd)
  results$values <- sapply(objective_functions_list, function(f) { f(results$best_solution) } )
  results$weighted_values <- results$values * weights
  return(results)
}
