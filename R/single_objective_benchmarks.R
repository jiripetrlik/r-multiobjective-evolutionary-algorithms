#' Rastrigin benchmark function
#'
#' Rastrigin benchmark function. For more info see
#' \url{https://en.wikipedia.org/wiki/Test_functions_for_optimization}.
#' @param v Vector with input parameters
#' @return Function value
#'
#' @export
rastrigin_function <- function(v) {
  n <- length(v)
  value <- 10 * n + sum(v^2 - 10 * cos(2 * pi * v))

  return(value)
}

#' Sphere benchmark function
#'
#' Sphere benchmark function. For more info see
#' \url{https://en.wikipedia.org/wiki/Test_functions_for_optimization}.
#' @param v Vector with input parameters
#' @return Function value
#'
#' @export
sphere_function <- function(v) {
  value <- sum(v^2)

  return(value)
}

#' Himmelblau benchmark function
#'
#' Himmelblau benchmark function. For more info see
#' \url{https://en.wikipedia.org/wiki/Test_functions_for_optimization}.
#' @param v Vector with input parameters
#' @return Function value
#'
#'  @export
himmelblau_function <- function(v) {
  if (length(v) != 2) {
    stop("Himmelblau function must have 2 parameters")
  }

  value <- (v[1]^2 + v[2] - 11)^2 + (v[1] + v[2]^2 -7)^2

  return(value)
}

#' Easom benchmark function
#'
#' Easom benchmark function. For more info see
#' \url{https://en.wikipedia.org/wiki/Test_functions_for_optimization}.
#' @param v Vector with input parameters
#' @return Function value
#'
#' @export
easom_function <- function(v) {
  if (length(v) != 2) {
    stop("Easom function must have 2 parameters")
  }

  value <- -cos(v[1]) * cos(v[2]) * exp(-((v[1] - pi)^2 + (v[2] - pi)^2))
  return(value)
}
