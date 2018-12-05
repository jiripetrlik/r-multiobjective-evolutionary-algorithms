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
