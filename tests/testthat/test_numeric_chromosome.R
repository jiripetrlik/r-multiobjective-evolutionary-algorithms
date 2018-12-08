library(testthat)
library(rmoea)

context("Numeric chromosome tests")

test_that("Init numeric chromosome test", {
  set.seed(1)
  size <- 1000
  chromosome <- init_numeric_chromosome(size)
  expect_true(is.numeric(chromosome))
  expect_length(chromosome, size)
  expect_true(all(chromosome >= 0))
  expect_true(all(chromosome <= 1))
})

test_that("Simulated binary crossover test", {
  set.seed(1)
  size <- 1000
  parent1 <- init_numeric_chromosome(size)
  parent2 <- init_numeric_chromosome(size)

  children <- simulated_binary_crossover(parent1, parent2)

  expect_length(children, 2)

  expect_true(is.numeric(children$child1))
  expect_true(is.numeric(children$child2))

  expect_length(children$child1, size)
  expect_length(children$child2, size)

  expect_true(all(children$child1 >= 0))
  expect_true(all(children$child2 >= 0))

  expect_true(all(children$child1 <= 1))
  expect_true(all(children$child2 <= 1))
})

test_that("Normally distributed mutation test", {
  set.seed(1)
  size <- 1000
  chromosome <- init_numeric_chromosome(size)
  chromosome <- normally_distributed_mutation(chromosome)
  expect_true(is.numeric(chromosome))
  expect_length(chromosome, size)
  expect_true(all(chromosome >= 0))
  expect_true(all(chromosome <= 1))

  chromosome <- rep(0, size)
  chromosome <- normally_distributed_mutation(chromosome)
  expect_true(all(chromosome >= 0))

  chromosome <- rep(1, size)
  chromosome <- normally_distributed_mutation(chromosome)
  expect_true(all(chromosome <= 1))
})

test_that("Test numeric chromosome scale 1.", {
  chromosome <- c(0, 1, 0.5, 0.25, 0.75)
  scaled_values <- scale_numeric_chromosome(chromosome, -2, 2)
  expect_true(all.equal.numeric(scaled_values, c(-2, 2, 0, -1, 1)))
})

test_that("Test numeric chromosome scale 1.", {
  chromosome <- c(0, 0, 0, 1, 1, 1)
  min <- c(0, -2, 1, 0, -2, 1)
  max <- c(1, 2, 5, 1, 2, 5)
  scaled_values <- scale_numeric_chromosome(chromosome, min, max)

  expect_true(all.equal.numeric(scaled_values, c(0, -2, 1, 1, 2, 5)))
})
