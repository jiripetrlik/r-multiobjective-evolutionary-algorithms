library(testthat)
library(rmoea)

context("NSGAII tests")

test_that("Non-dominated sorting", {
  m <- matrix(c(
    1, 1, 2,
    2, 3, 2,
    2, 2, 3,
    5, 4, 5,
    2, 1, 1
  ), nrow = 5 , byrow = TRUE);

  result <- nondominated_sort(m)
  expect_equal(length(result), 5)
  expect_equal(result, c(1, 2, 2, 3, 1))
})

test_that("Crowding distance assignment - 1 solution", {
  m <- matrix(c(
    1, 1, 2
  ), nrow = 1 , byrow = TRUE);

  result <- crowding_distance_assignment(m)
  expect_equal(length(result), 1)
  expect_equal(result, Inf)
})

test_that("Crowding distance assignment - 2 solutions", {
  m <- matrix(c(
    1, 1, 2,
    1, 2, 2
  ), nrow = 2 , byrow = TRUE);

  result <- crowding_distance_assignment(m)
  expect_equal(length(result), 2)
  expect_equal(result, c(Inf, Inf))
})

test_that("Crowding distance assignment - multiple solutions", {
  m <- matrix(c(
    2, 16, 2,
    1, 2, 1,
    3, 6, 3,
    11, 22, 11,
    8, 4, 8
  ), nrow = 5 , byrow = TRUE);

  result <- crowding_distance_assignment(m)
  expect_equal(length(result), 5)
  expect_equal(result, c(1.2, Inf, 1.8, Inf, 1.8))
})

test_that("Test binary NSGAII", {
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
  
  results <- nsga2(functions, number_of_iterations = number_of_iterations, chromosome_size = 24, chromosome_type = "binary")
  expect_gt(nrow(results$values), 2)
  expect_lt(min(results$values[, 1]), 0.02)
  expect_lt(min(results$values[, 2]), 0.02)
})

test_that("Test binary NSGAII parameters", {
  f1 <- function(ch) {
    x <- binary_chromosome_to_numeric(ch, -10, 10)
    return(abs(x + 0.1))
  }
  f2 <- function(ch) {
    x <- binary_chromosome_to_numeric(ch, -10, 10)
    return(abs(x + 0.2))
  }
  functions <- list(f1, f2)
  
  chromosome_size <- 16
  chromosome_type <- "binary"
  population_size <- 50
  number_of_iterations <- 10
  mutation_probability <- 0.1
  
  results <- nsga2(functions,
                  chromosome_size = chromosome_size,
                  chromosome_type = chromosome_type,
                  population_size = population_size,
                  number_of_iterations = number_of_iterations,
                  mutation_probability = mutation_probability);
  
  expect_length(results$parameters$objective_functions_list, 2)
  expect_equal(results$parameters$chromosome_size, chromosome_size)
  expect_equal(results$parameters$chromosome_type, chromosome_type)
  expect_equal(results$parameters$population_size, population_size)
  expect_equal(results$parameters$number_of_iterations, number_of_iterations)
  expect_equal(results$parameters$mutation_probability, mutation_probability)
})