library(testthat)
library(rmoea)

context("Single objective genetic algorithm tests")

test_that("Test binary chromosome GA using quadratic benchmark", {
  set.seed(1)

  objective_function <- function(chromosome) {
    x1 <- binary_chromosome_to_numeric(chromosome[1:16], -10, 10)
    x2 <- binary_chromosome_to_numeric(chromosome[17:32], -10, 10)

    return(quadratic_benchmark_function(c(x1 - 1, x2 + 1)))
  }

  results <- single_objective_ga(objective_function, 32)
  expect_true(is.list(results))
  expect_lt(results$value, 0.01)
  expect_lt(abs(binary_chromosome_to_numeric(results$best_solution[1:16], -10, 10) - 1), 0.1)
  expect_lt(abs(binary_chromosome_to_numeric(results$best_solution[17:32], -10, 10) + 1), 0.1)
})
