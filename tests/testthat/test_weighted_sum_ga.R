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

  results <- weighted_sum_ga(functions, weights, 32, "binary")

  expect_lt(abs(results$value - 4), 0.1)
  expect_lt(abs(binary_chromosome_to_numeric(results$best_solution[1:16], -10, 10) - 1), 0.1)
  expect_lt(abs(binary_chromosome_to_numeric(results$best_solution[17:32], -10, 10) + 1), 0.1)
  expect_lt(abs(results$values[1] - 2), 0.1)
  expect_lt(abs(results$values[2] - 1), 0.1)
  expect_lt(abs(results$weighted_values[1] - 2), 0.1)
  expect_lt(abs(results$weighted_values[2] - 2), 0.1)
})

test_that("Test numeric weighted sum GA", {
  f1 <- function(ch) {
    ch <- scale_numeric_chromosome(ch, -10, 10)
    return((ch[1] - 1) ^ 2 + 2)
  }
  f2 <- function(ch) {
    ch <- scale_numeric_chromosome(ch, -10, 10)
    return((ch[2] + 1) ^ 2 + 1)
  }
  functions <- list(f1, f2)
  weights <- c(1, 2)

  results <- weighted_sum_ga(functions, weights, 2, "numeric")

  expect_lt(abs(results$value - 4), 0.1)
  expect_lt(abs(scale_numeric_chromosome(results$best_solution[1], -10, 10) - 1), 0.1)
  expect_lt(abs(scale_numeric_chromosome(results$best_solution[2], -10, 10) + 1), 0.1)
  expect_lt(abs(results$values[1] - 2), 0.1)
  expect_lt(abs(results$values[2] - 1), 0.1)
  expect_lt(abs(results$weighted_values[1] - 2), 0.1)
  expect_lt(abs(results$weighted_values[2] - 2), 0.1)
})
