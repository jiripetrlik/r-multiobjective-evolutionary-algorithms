#' @export
single_objective_ga <- function(objective_function,
                                chromosome_size,
                                chromosome_type = "binary",
                                population_size = 100,
                                number_of_iterations = 100,
                                elitism = TRUE,
                                mutation_probability = 0.05) {
  if (chromosome_type == "binary") {
    binary_single_objective_ga(objective_function, chromosome_size, population_size, number_of_iterations, mutation_probability)
  } else if (chromosome_type == "numeric") {
    numeric_single_objective_ga(objective_function, chromosome_size, population_size, number_of_iterations, mutation_probability)
  } else {
    stop("Unknown chromosome type")
  }
}

binary_single_objective_ga <- function(objective_function,
                                       chromosome_size,
                                       population_size,
                                       number_of_iterations,
                                       elitism = TRUE,
                                       mutation_probability) {
  population <- replicate(population_size, init_binary_chromosome(chromosome_size), simplify = FALSE)
  for (iteration in 1:number_of_iterations) {
    for (i in 1:(population_size / 2)) {
      parents <- random_integer(1, population_size, 2)
      children <- one_point_crossover(population[[parents[1]]], population[[parents[2]]])
      population[[population_size + i * 2 - 1]] <- children$child1
      population[[population_size + i * 2]] <- children$child2
    }
    population[(population_size + 1):(2 * population_size)] <- lapply(population[(population_size + 1):(2 * population_size)], binaryMutation)
    fitness_values <- sapply(population, objective_function)
    selected <- tournament_selection(fitness_values)
    if (elitism == TRUE) {
      selected[1] <- which.min(fitness_values)
    }
    population <- population[selected]
  }
  fitness_values <-fitness_values[selected]
  best_solution_index <- which.min(fitness_values)

  results <- list()
  results$value <- fitness_values[best_solution_index]
  results$best_solution <- population[[best_solution_index]]
  results$best_solution_index <- best_solution_index

  return(results)
}

numeric_single_objective_ga <- function(objective_function,
                                        chromosome_size,
                                        population_size,
                                        number_of_iterations,
                                        elitism = TRUE,
                                        mutation_probability) {
  population <- replicate(population_size, init_numeric_chromosome(chromosome_size), simplify = FALSE)
  for (iteration in 1:number_of_iterations) {
    for (i in 1:(population_size / 2)) {
      parents <- random_integer(1, population_size, 2)
      children <- simulated_binary_crossover(population[[parents[1]]], population[[parents[2]]])
      population[[population_size + i * 2 - 1]] <- children$child1
      population[[population_size + i * 2]] <- children$child2
    }
    population[(population_size + 1):(2 * population_size)] <- lapply(population[(population_size + 1):(2 * population_size)], normally_distributed_mutation)
    fitness_values <- sapply(population, objective_function)
    selected <- tournament_selection(fitness_values)
    if (elitism == TRUE) {
      selected[1] <- which.min(fitness_values)
    }
    population <- population[selected]
  }
  fitness_values <-fitness_values[selected]
  best_solution_index <- which.min(fitness_values)

  results <- list()
  results$value <- fitness_values[best_solution_index]
  results$best_solution <- population[[best_solution_index]]
  results$best_solution_index <- best_solution_index

  return(results)
}
