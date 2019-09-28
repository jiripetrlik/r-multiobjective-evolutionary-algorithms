#' @export
quadratic_benchmark_function <- function(x) {
  return(sum(x^2))
}

#' Fonseca-Fleming benchmark function
#'
#' Fonseca-Fleming benchmark function. For more info see
#' \url{https://en.wikipedia.org/wiki/Test_functions_for_optimization}.
#' @param v Vector with input parameters
#' @return Vector with two objective functions values
#'
#' @export
fonseca_fleming_function <- function(v) {
  n <- 1:length(v)
  value1 <- 1 - exp(-sum((v - 1 / sqrt(n))^2))
  value2 <- 1 - exp(-sum((v + 1 / sqrt(n))^2))

  return(c(value1, value2))
}

#' Kursawe benchmark function
#'
#' Kursawe benchmark function. For more info see
#' \url{https://en.wikipedia.org/wiki/Test_functions_for_optimization}.
#' @param v Vector with input parameters
#' @return Vector with two objective functions values
#'
#' @export
kursawe_function <- function(v) {
  if (length(v) != 3) {
    stop("Kursawe function must have 3 parameters")
  }

  tmpV1 <- v[1:2]
  tmpV2 <- v[2:3]
  value1 <- sum(-10 * exp(-0.2 * sqrt(tmpV1^2 + tmpV2^2)))
  value2 <- sum(abs(v)^0.8 + 5 * sin(v^3))

  return(c(value1, value2))
}

#' Schaffer function 1.
#'
#' Schaffer benchmark function 1. For more info see
#' \url{https://en.wikipedia.org/wiki/Test_functions_for_optimization}.
#' @param v Vector with input parameters
#' @return Vector with two objective functions values
#'
#' @export
schaffer_function <- function(x) {
  if (length(x) != 1) {
    stop("Schaffer function must have only 1 parameter")
  }

  value1 <- x^2
  value2 <- (x - 2)^2

  return(c(value1, value2))
}
