pareto_dominates_fast <- function(a, b) {
  return(all(a <= b) && any(a < b))
}

#' @export
pareto_dominates <- function(a, b, minimize = TRUE) {
  if (is.numeric(a) == FALSE) {
    stop("First parameter must be a numeric value")
  }
  if (is.numeric(b) == FALSE) {
    stop("Second parameter must be a numeric value")
  }

  if (minimize == FALSE) {
    a <- (-a)
    b <- (-b)
  }

  return(pareto_dominates_fast(a, b))
}

check_objective_vectors_list <- function(l) {
  if (any(sapply(l, function(x) { is.numeric(x) == FALSE } ))) {
    stop("All items in objective vector list must be numeric vectors")
  }

  if (length(l) > 0) {
    size  <- length(l[[1]])
    if (all(sapply(l, function(x) { length(x) == size })) == FALSE) {
      stop("All vectors in objective vector list must have the same size")
    }
  }
}

is_nondominated_fast <- function(x, solutions, minimize) {
  return(any(sapply(solutions, function(s) { pareto_dominates(s, x, minimize) })) == FALSE)
}

#' @export
is_nondominated <- function(x, solutions, minimize = TRUE) {
  check_objective_vectors_list(solutions)
  return(is_nondominated_fast(x, solutions, minimize))
}

find_nondominated_fast <- function(solutions, minimize) {
  return(sapply(solutions, function(s) {is_nondominated(s, solutions, minimize)}))
}

#' @export
find_nondominated <- function(solutions, minimize = TRUE) {
  if (is.list(solutions)) {
    check_objective_vectors_list(solutions)
    return(find_nondominated_fast(solutions, minimize))
  } else if(is.matrix(solutions)) {
    number_of_solutions <- nrow(solutions)
    objective_functions_number <- ncol(solutions)
    solutions <- t(solutions)
    solutions <- split(as.vector(solutions), rep(1:number_of_solutions, each = objective_functions_number))
    return(find_nondominated_fast(solutions, minimize))
  } else {
    stop("Parameter solutions must be list or matrix")
  }
}
