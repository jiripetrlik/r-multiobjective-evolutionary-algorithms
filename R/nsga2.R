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
