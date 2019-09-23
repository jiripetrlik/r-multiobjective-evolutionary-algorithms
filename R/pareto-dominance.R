pareto_dominates_fast <- function(a, b) {
  return(all(a <= b) && any(a < b))
}

#' Check pareto dominance
#'
#' Check whether solution \code{a} dominates solution \code{b}
#' @param a Numeric vector with values of objective functions for solution \code{a}
#' @param b Numeric vector with values of objective functions for solution \code{b}
#' @param minimize If objective functions should be minimized
#' @return Logical value
#'
#' @examples
#' solutionA <- c(1,2,2)
#' solutionB <- c(5,2,2)
#' pareto_dominates(solutionA, solutionB)
#'
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

convert_objective_matrix_to_list <- function(m) {
  l <- split(t(m), rep(1:nrow(m), each=ncol(m)))
  
  return(l)
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

#' Check if solution in nondominated
#'
#' Check if solution \code{x} is not dominated by any other solution in the set \code{solutions}
#' @param x Numeric vector with objective functions values for solution \code{x}
#' @param solutions List or matrix with objective functions values for \code{solutions}.
#' In matrix each row contains values for one solution
#' @param minimize If objective functions should be minimized
#' @return Logical value
#'
#' @examples
#' x <- c(1, 2, 2)
#' solutions <- list(c(1, 5, 5), c(8, 4, 8), c(3, 2, 2))
#' is_nondominated(x, solutions)
#'
#' @export
is_nondominated <- function(x, solutions, minimize = TRUE) {
  if (is.matrix(solutions)) {
    solutions <- convert_objective_matrix_to_list(solutions)
  }
  
  check_objective_vectors_list(solutions)
  return(is_nondominated_fast(x, solutions, minimize))
}

find_nondominated_fast <- function(solutions, minimize) {
  return(sapply(solutions, function(s) {is_nondominated(s, solutions, minimize)}))
}

#' Find nondominated solutions
#'
#' Find solutions which are not dominated by any other solution
#' @param solutions List or matrix with objective functions values for \code{solutions}.
#' In matrix each row contains values for one solution
#' @param minimize If objective functions should be minimized
#' @return Logical vector with the same length as number of solutions. Vector contains
#' TRUE for solutions which are nondominated.
#'
#' @examples
#' solutions <- list(c(1, 5, 5), c(8, 4, 8), c(3, 2, 2))
#' find_nondominated(solutions)
#'
#' @export
find_nondominated <- function(solutions, minimize = TRUE) {
  if (is.matrix(solutions)) {
    solutions <- convert_objective_matrix_to_list(solutions)
  }
  
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
