nondominated_sort <- function(objective_functions_values) {
  number_of_solutions <- nrow(objective_functions_values)

  sp <- rep(list(numeric()), number_of_solutions)
  np <- rep(0, number_of_solutions)
  for (i in 1:number_of_solutions) {
    for (j in 1:number_of_solutions) {
      if (pareto_dominates_fast(objective_functions_values[i,], objective_functions_values[j,])) {
        sp[[i]] <- c(sp[[i]], j)
      }
      if (pareto_dominates_fast(objective_functions_values[j,], objective_functions_values[i,])) {
        np[i] <- np[i] + 1
      }
    }
  }

  solution_rank <- rep(-1, number_of_solutions)
  i <- 1
  while(0 %in% np) {
    current_pareto_front <- which(np == 0)
    np[current_pareto_front] <- -1
    solution_rank[current_pareto_front] <- i

    for (j in current_pareto_front) {
      np[sp[[j]]] <- np[sp[[j]]] - 1
    }

    i <- i + 1
  }

  return(solution_rank)
}

crowding_distance_assignment <- function(objective_functions_values) {
  number_of_solutions <- nrow(objective_functions_values)
  if (number_of_solutions <= 2) {
    return(rep(Inf, number_of_solutions))
  }

  number_of_objectives <- ncol(objective_functions_values)
  distance <- rep(0, number_of_solutions)

  for (i in 1:number_of_objectives) {
    ord <- order(objective_functions_values[, i])
    rk <- rank(objective_functions_values[, i], ties.method = "first")
    values <- objective_functions_values[ord, i]
    minimum <- min(values)
    maximum <- max(values)
    r <- maximum - minimum

    tmpDistance <- (values[3:length(values)] - values[1:(length(values) - 2)]) / r
    tmpDistance <- c(Inf, tmpDistance, Inf)
    distance <- distance + tmpDistance[rk]
  }

  return(distance)
}

evaluate_objective_functions <- function(solutions, objective_functions_list) {
  number_of_objective_functions <- length(objective_functions_list)
  objective_functions_values <- numeric()
  for (i in (1 : number_of_objective_functions)) {
    objective_functions_values <- c(objective_functions_values, sapply(solutions, objective_functions_list[[i]]))
  }
  objective_functions_values <- matrix(objective_functions_values, ncol = number_of_objective_functions)

  return(objective_functions_values)
}

#' @export
nsga2 <- function(objective_functions_list,
                  chromosome_size,
                  chromosome_type = "binary",
                  population_size = length(objective_functions_list) * 100,
                  number_of_iterations = 100,
                  nc = 2,
                  mutation_probability = 0.05,
                  uniform_mutation_sd = 0.01) {
  if (chromosome_type == "binary") {
    binary_nsga2(objective_functions_list = objective_functions_list,
                 chromosome_size = chromosome_size,
                 population_size = population_size,
                 number_of_iterations = number_of_iterations,
                 mutation_probability = mutation_probability)
  } else if (chromosome_type == "numeric") {
    numeric_nsga2(objective_functions_list = objective_functions_list,
                 chromosome_size = chromosome_size,
                 population_size = population_size,
                 number_of_iterations = number_of_iterations,
                 nc = nc,
                 uniform_mutation_sd = uniform_mutation_sd)
  } else {
    stop("Unknown chromosome type")
  }
}

binary_nsga2 <- function(objective_functions_list,
                         chromosome_size,
                         population_size,
                         number_of_iterations,
                         mutation_probability = 0.05) {
  number_of_objective_functions <- length(objective_functions_list)
  p <- replicate(population_size, init_binary_chromosome(chromosome_size), simplify = FALSE)
  p_objective_functions_values <- evaluate_objective_functions(p, objective_functions_list)
  statistics <- list(min_fitness = list(), max_fitness = list(), mean_fitness = list(), sd_fitness = list())
  for (i in 1:number_of_objective_functions) {
    statistics$min_fitness[[i]] <- numeric()
    statistics$max_fitness[[i]] <- numeric()
    statistics$mean_fitness[[i]] <- numeric()
    statistics$sd_fitness[[i]] <- numeric()
  }
  
  for (iteration in 1:number_of_iterations) {
    q <- list()
    for (i in 1:(population_size / 2)) {
      parents <- random_integer(1, population_size, 2)
      children <- one_point_crossover(p[[parents[1]]], p[[parents[2]]])
      q[[i * 2 - 1]] <- children$child1
      q[[i * 2]] <- children$child2
    }
    q[1:population_size] <- lapply(q[1:population_size], bind_parameters(binaryMutation, probability = mutation_probability))

    # Evaluate objective functions for Q
    q_objective_functions_values <- evaluate_objective_functions(q, objective_functions_list)
    
    pq <- c(p, q)
    objective_functions_values <- rbind(p_objective_functions_values, q_objective_functions_values)
    r <- nondominated_sort(objective_functions_values)
    o <- order(r)
    r <- r[o]
    pq <- pq[o]
    objective_functions_values <- objective_functions_values[o,]
    
    if (r[population_size] == r[population_size + 1]) {
      fi_r <- r[population_size]
      fi <- which(r == fi_r)
      cda <- crowding_distance_assignment(objective_functions_values[fi,])
      o_fi <- fi[order(cda, decreasing = TRUE)]
      
      fi_from <- min(fi)
      fi_to <- max(fi)
      r[fi_from : fi_to] <- r[o_fi]
      pq[fi_from : fi_to] <- pq[o_fi]
      objective_functions_values[fi_from : fi_to,] <- objective_functions_values[o_fi,]
    }
    
    p <- pq[1 : population_size]
    p_objective_functions_values <- objective_functions_values[1 : population_size,]
    
    for (i in 1:number_of_objective_functions) {
      statistics$min_fitness[[i]] <- c(statistics$min_fitness[[i]], min(p_objective_functions_values[, i]))
      statistics$max_fitness[[i]] <- c(statistics$max_fitness[[i]], max(p_objective_functions_values[, i]))
      statistics$mean_fitness[[i]] <- c(statistics$mean_fitness[[i]], mean(p_objective_functions_values[, i]))
      statistics$sd_fitness[[i]] <- c(statistics$sd_fitness[[i]], sd(p_objective_functions_values[, i]))
    }
  }
  
  nondominated <- find_nondominated(p_objective_functions_values)
  results <- list()
  results$values <- p_objective_functions_values[nondominated,]
  results$nondominated_solutions <- p[nondominated]
  results$statistics <- statistics
  
  parameters <- list()
  parameters$objective_functions_list <- objective_functions_list
  parameters$chromosome_type <- "binary"
  parameters$chromosome_size <- chromosome_size
  parameters$population_size <- population_size
  parameters$number_of_iterations <- number_of_iterations
  parameters$mutation_probability <- mutation_probability
  results$parameters <- parameters
  
  return(results)
}

numeric_nsga2 <- function(objective_functions_list,
                         chromosome_size,
                         population_size,
                         number_of_iterations,
                         nc,
                         uniform_mutation_sd) {
  number_of_objective_functions <- length(objective_functions_list)
  p <- replicate(population_size, init_numeric_chromosome(chromosome_size), simplify = FALSE)
  p_objective_functions_values <- evaluate_objective_functions(p, objective_functions_list)
  statistics <- list(min_fitness = list(), max_fitness = list(), mean_fitness = list(), sd_fitness = list())
  for (i in 1:number_of_objective_functions) {
    statistics$min_fitness[[i]] <- numeric()
    statistics$max_fitness[[i]] <- numeric()
    statistics$mean_fitness[[i]] <- numeric()
    statistics$sd_fitness[[i]] <- numeric()
  }
  
  for (iteration in 1:number_of_iterations) {
    q <- list()
    for (i in 1:(population_size / 2)) {
      parents <- random_integer(1, population_size, 2)
      children <- simulated_binary_crossover(p[[parents[1]]], p[[parents[2]]], nc)
      q[[i * 2 - 1]] <- children$child1
      q[[i * 2]] <- children$child2
    }
    q[1:population_size] <- lapply(q[1:population_size], bind_parameters(
      normally_distributed_mutation, sd = uniform_mutation_sd))
    
    # Evaluate objective functions for Q
    q_objective_functions_values <- evaluate_objective_functions(q, objective_functions_list)
    
    pq <- c(p, q)
    objective_functions_values <- rbind(p_objective_functions_values, q_objective_functions_values)
    r <- nondominated_sort(objective_functions_values)
    o <- order(r)
    r <- r[o]
    pq <- pq[o]
    objective_functions_values <- objective_functions_values[o,]
    
    if (r[population_size] == r[population_size + 1]) {
      fi_r <- r[population_size]
      fi <- which(r == fi_r)
      cda <- crowding_distance_assignment(objective_functions_values[fi,])
      o_fi <- fi[order(cda, decreasing = TRUE)]
      
      fi_from <- min(fi)
      fi_to <- max(fi)
      r[fi_from : fi_to] <- r[o_fi]
      pq[fi_from : fi_to] <- pq[o_fi]
      objective_functions_values[fi_from : fi_to,] <- objective_functions_values[o_fi,]
    }
    
    p <- pq[1 : population_size]
    p_objective_functions_values <- objective_functions_values[1 : population_size,]
    
    for (i in 1:number_of_objective_functions) {
      statistics$min_fitness[[i]] <- c(statistics$min_fitness[[i]], min(p_objective_functions_values[, i]))
      statistics$max_fitness[[i]] <- c(statistics$max_fitness[[i]], max(p_objective_functions_values[, i]))
      statistics$mean_fitness[[i]] <- c(statistics$mean_fitness[[i]], mean(p_objective_functions_values[, i]))
      statistics$sd_fitness[[i]] <- c(statistics$sd_fitness[[i]], sd(p_objective_functions_values[, i]))
    }
  }
  
  nondominated <- find_nondominated(p_objective_functions_values)
  results <- list()
  results$values <- p_objective_functions_values[nondominated,]
  results$nondominated_solutions <- p[nondominated]
  results$statistics <- statistics
  
  parameters <- list()
  parameters$objective_functions_list <- objective_functions_list
  parameters$chromosome_type <- "numeric"
  parameters$chromosome_size <- chromosome_size
  parameters$population_size <- population_size
  parameters$number_of_iterations <- number_of_iterations
  parameters$nc <- nc
  parameters$uniform_mutation_sd <- uniform_mutation_sd
  results$parameters <- parameters
  
  return(results)
}
