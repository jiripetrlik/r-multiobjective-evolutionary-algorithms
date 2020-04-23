library(testthat)
library(rmoea)

context("Weighted sum GA tests")

test_that("Test weigted sum function", {
  f1 <- function(ch) { ch[1] + ch[2] + 1 }
  f2 <- function(ch) { ch[1] ^ 2 - ch[2] ^ 2 }
  f3 <- function(ch) { ch[1] - 5 }
  functions <- list(f1, f2, f3)
  weights <- c(5, 1, 10)

  expect_equal(weighted_sum_function(c(3, 5), functions, weights), 9)
})

test_that("Test binary weighted sum GA", {
  f1 <- function(ch) {
    x <- binary_chromosome_to_numeric(ch[1:16], -10, 10)
    return((x - 1) ^ 2 + 2)
  }
  f2 <- function(ch) {
    x <- binary_chromosome_to_numeric(ch[17:32], -10, 10)
    return((x + 1) ^ 2 + 1)
  }
  functions <- list(f1, f2)
  weights <- c(1, 2)

  results <- weighted_sum_ga(functions, weights, "binary", nBits = 32)

  expect_lt(abs(results$value - 4), 0.1)
  expect_lt(abs(binary_chromosome_to_numeric(results$best_solution[1:16], -10, 10) - 1), 0.1)
  expect_lt(abs(binary_chromosome_to_numeric(results$best_solution[17:32], -10, 10) + 1), 0.1)
  expect_lt(abs(results$values[1] - 2), 0.1)
  expect_lt(abs(results$values[2] - 1), 0.1)
  expect_lt(abs(results$weighted_values[1] - 2), 0.1)
  expect_lt(abs(results$weighted_values[2] - 2), 0.1)
})

test_that("Test real-valued weighted sum GA", {
  f1 <- function(ch) {
    return((ch[1] - 1) ^ 2 + 2)
  }
  f2 <- function(ch) {
    return((ch[2] + 1) ^ 2 + 1)
  }
  functions <- list(f1, f2)
  weights <- c(1, 2)

  results <- weighted_sum_ga(functions, weights, "real-valued", lower = c(-10, -10), upper = c(10, 10))

  expect_lt(abs(results$value - 4), 0.1)
  expect_lt(abs(results$best_solution[1] - 1), 0.1)
  expect_lt(abs(results$best_solution[2] + 1), 0.1)
  expect_lt(abs(results$values[1] - 2), 0.1)
  expect_lt(abs(results$values[2] - 1), 0.1)
  expect_lt(abs(results$weighted_values[1] - 2), 0.1)
  expect_lt(abs(results$weighted_values[2] - 2), 0.1)
})

test_that("Test binary chromosome weighted sum GA parameters", {
  f1 <- function(ch) {
    x <- binary_chromosome_to_numeric(ch[1:16], -10, 10)
    return((x - 1) ^ 2 + 2)
  }
  f2 <- function(ch) {
    x <- binary_chromosome_to_numeric(ch[17:32], -10, 10)
    return((x + 1) ^ 2 + 1)
  }
  functions <- list(f1, f2)
  weights <- c(1, 2)
  nBits <- 32
  chromosome_type <- "binary"
  population_size <- 50
  number_of_iterations <- 10
  elitism <- FALSE
  mutation_probability <- 0.1

  results <- weighted_sum_ga(functions,
                             weights,
                             chromosome_type = chromosome_type,
                             nBits = nBits,
                             population_size = population_size,
                             number_of_iterations = number_of_iterations,
                             elitism = elitism,
                             mutation_probability = mutation_probability);

  expect_true(all(results$parameters$weights == weights))
  expect_true(is.list(results$parameters$objective_functions_list))
  expect_length(results$parameters$objective_functions_list, 2)
  expect_equal(results$parameters$nBits, nBits)
  expect_equal(results$parameters$chromosome_type, chromosome_type)
  expect_equal(results$parameters$population_size, population_size)
  expect_equal(results$parameters$number_of_iterations, number_of_iterations)
  expect_equal(results$parameters$elitism, elitism)
  expect_equal(results$parameters$mutation_probability, mutation_probability)
})

test_that("Test numeric chromosome weighted sum GA parameters", {
  f1 <- function(ch) {
    return((ch[1] - 1) ^ 2 + 2)
  }
  f2 <- function(ch) {
    return((ch[2] + 1) ^ 2 + 1)
  }
  functions <- list(f1, f2)
  weights <- c(1, 2)
  lower <- c(-10, -10)
  upper <- c(10, 10)
  chromosome_size <- 2
  chromosome_type <- "real-valued"
  population_size <- 50
  number_of_iterations <- 10
  elitism <- FALSE
  nc <- 5
  uniform_mutation_sd <- 0.1

  results <- weighted_sum_ga(functions,
                             weights,
                             chromosome_type = chromosome_type,
                             lower = lower,
                             upper = upper,
                             population_size = population_size,
                             number_of_iterations = number_of_iterations,
                             elitism = elitism,
                             nc = nc,
                             uniform_mutation_sd = uniform_mutation_sd);

  expect_true(all(results$parameters$weights == weights))
  expect_true(is.list(results$parameters$objective_functions_list))
  expect_length(results$parameters$objective_functions_list, 2)
  expect_equal(results$parameters$lower, lower)
  expect_equal(results$parameters$upper, upper)
  expect_equal(results$parameters$chromosome_type, chromosome_type)
  expect_equal(results$parameters$population_size, population_size)
  expect_equal(results$parameters$number_of_iterations, number_of_iterations)
  expect_equal(results$parameters$elitism, elitism)
  expect_equal(results$parameters$nc, nc)
  expect_equal(results$parameters$uniform_mutation_sd, uniform_mutation_sd)
})
