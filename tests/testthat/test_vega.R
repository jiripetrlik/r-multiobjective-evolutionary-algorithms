library(testthat)
library(rmoea)

context("VEGA tests")

test_that("Test binary VEGA", {
  set.seed(1)

  number_of_iterations = 100
  f1 <- function(ch) {
    x <- binary_chromosome_to_numeric(ch, -10, 10)
    return(abs(x + 0.1))
  }
  f2 <- function(ch) {
    x <- binary_chromosome_to_numeric(ch, -10, 10)
    return(abs(x + 0.2))
  }
  functions <- list(f1, f2)

  results <- vega(functions, number_of_iterations = number_of_iterations, nBits = 24, chromosome_type = "binary")
  expect_gt(nrow(results$values), 2)
  expect_lt(min(results$values[, 1]), 0.02)
  expect_lt(min(results$values[, 2]), 0.02)

  expect_true(all(c("min_fitness", "mean_fitness", "max_fitness", "sd_fitness") %in% names(results$statistics)))
  expect_length(results$statistics$min_fitness, length(functions))
  expect_length(results$statistics$max_fitness[[1]], number_of_iterations)
})

test_that("Test numeric VEGA", {
  set.seed(1)

  number_of_iterations = 1000
  f1 <- function(ch) {
    return(abs(ch - 0.1))
  }
  f2 <- function(ch) {
    return(abs(ch + 0.1))
  }
  functions <- list(f1, f2)

  results <- vega(functions, lower = -10, upper = 10, number_of_iterations = number_of_iterations, chromosome_type = "real-valued")
  expect_gt(nrow(results$values), 2)
  expect_lt(min(results$values[, 1]), 0.02)
  expect_lt(min(results$values[, 2]), 0.02)

  expect_true(all(c("min_fitness", "mean_fitness", "max_fitness", "sd_fitness") %in% names(results$statistics)))
  expect_length(results$statistics$min_fitness, length(functions))
  expect_length(results$statistics$max_fitness[[1]], number_of_iterations)
})

test_that("Test binary VEGA parameters", {
  f1 <- function(ch) {
    x <- binary_chromosome_to_numeric(ch, -10, 10)
    return(abs(x + 0.1))
  }
  f2 <- function(ch) {
    x <- binary_chromosome_to_numeric(ch, -10, 10)
    return(abs(x + 0.2))
  }
  functions <- list(f1, f2)

  nBits <- 16
  chromosome_type <- "binary"
  population_size <- 50
  number_of_iterations <- 10
  mutation_probability <- 0.1

  results <- vega(functions,
                  chromosome_type = chromosome_type,
                  nBits = nBits,
                  population_size = population_size,
                  number_of_iterations = number_of_iterations,
                  mutation_probability = mutation_probability);

  expect_length(results$parameters$objective_functions_list, 2)
  expect_equal(results$parameters$chromosome_type, chromosome_type)
  expect_equal(results$parameters$nBits, nBits)
  expect_equal(results$parameters$population_size, population_size)
  expect_equal(results$parameters$number_of_iterations, number_of_iterations)
  expect_equal(results$parameters$mutation_probability, mutation_probability)
})

test_that("Test numeric VEGA parameters", {
  f1 <- function(ch) {
    return(abs(ch - 0.1))
  }
  f2 <- function(ch) {
    return(abs(ch + 0.1))
  }
  functions <- list(f1, f2)

  lower <- -10
  upper <- 10
  chromosome_type <- "real-valued"
  population_size <- 50
  number_of_iterations <- 10
  nc <- 5
  uniform_mutation_sd <- 0.1

  results <- vega(functions,
                  chromosome_type = chromosome_type,
                  lower = lower,
                  upper = upper,
                  population_size = population_size,
                  number_of_iterations = number_of_iterations,
                  nc = nc,
                  uniform_mutation_sd = uniform_mutation_sd)

  expect_length(results$parameters$objective_functions_list, 2)
  expect_equal(results$parameters$lower, lower)
  expect_equal(results$parameters$upper, upper)
  expect_equal(results$parameters$chromosome_type, chromosome_type)
  expect_equal(results$parameters$population_size, population_size)
  expect_equal(results$parameters$number_of_iterations, number_of_iterations)
  expect_equal(results$parameters$nc, nc)
  expect_equal(results$parameters$uniform_mutation_sd, uniform_mutation_sd)
})
