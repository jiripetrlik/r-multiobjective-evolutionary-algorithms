library(testthat)
library(rmoea)

context("Binary chromosome tests")

test_that("Binary chromosome is a boolean vector", {
  chromosome <- init_binary_chromosome(100)
  expect_equal(class(chromosome), "logical")
})

test_that("Binary chromosome is as long as expected", {
  sizes <- c(5, 10, 100, 1000, 5000)
  real_sizes <- c()
  for (s in sizes) {
    chromosome <- init_binary_chromosome(s)
    real_sizes <- c(real_sizes, length(chromosome))
  }

  expect_equal(sizes, real_sizes)
})

test_that("Long binary chromosome contains both True and False values", {
  set.seed(1)
  chromosome <- init_binary_chromosome(10000)
  expect_true(all(c(TRUE, FALSE) %in% chromosome))
})

test_that("Single point crossover test", {
  size <- 100
  parent1 <- rep(TRUE, size)
  parent2 <- rep(FALSE, size)

  children <- one_point_crossover(parent1, parent2)

  # Check type
  expect_equal(class(children$child1), "logical")
  expect_equal(class(children$child2), "logical")

  # Check size
  expect_equal(length(children$child1), size)
  expect_equal(length(children$child2), size)

  # Check begin and end
  expect_equal(children$child1[1], TRUE)
  expect_equal(children$child1[size], FALSE)
  expect_equal(children$child2[1], FALSE)
  expect_equal(children$child2[size], TRUE)
})

test_that("Uniform crossover test", {
  set.seed(1)

  size <- 100
  parent1 <- rep(TRUE, size)
  parent2 <- rep(FALSE, size)

  children <- uniform_crossover(parent1, parent2)

  # Check type
  expect_equal(class(children$child1), "logical")
  expect_equal(class(children$child2), "logical")

  # Check size
  expect_equal(length(children$child1), size)
  expect_equal(length(children$child2), size)

  # Check that children contain part of both parents
  expect_true(expect_true(all(c(TRUE, FALSE) %in% children$child1)))
  expect_true(expect_true(all(c(TRUE, FALSE) %in% children$child2)))
})

test_that("Binary mutation test", {
  set.seed(1)
  size <- 10000
  parent <- rep(T, size)
  child <- binaryMutation(parent)

  expect_equal(class(child), "logical")
  expect_length(child, size)
  expect_true(all(c(TRUE, FALSE) %in% child))
  expect_true(sum(child == TRUE) > sum(child == FALSE))

  another_child <- binaryMutation(parent)
  expect_true(any(child != another_child))
})

test_that("Test binary chromosome conversion to default range", {
  min_chromosome <- rep(FALSE, 16)
  expect_equal(binary_chromosome_to_numeric(min_chromosome), 0)

  max_chromosome <- rep(TRUE, 16)
  expect_equal(binary_chromosome_to_numeric(max_chromosome), 1)

  small_value_chromosome <- rep(FALSE, 16)
  small_value_chromosome[1] <- TRUE
  expect_gt(binary_chromosome_to_numeric(small_value_chromosome), 0)
  expect_lt(binary_chromosome_to_numeric(small_value_chromosome), 0.1)

  high_value_chromosome <- rep(TRUE, 16)
  high_value_chromosome[1] <- FALSE
  expect_lt(binary_chromosome_to_numeric(high_value_chromosome), 1)
  expect_gt(binary_chromosome_to_numeric(high_value_chromosome), 0.9)
})

test_that("Test binary chromosome conversion to [-5,5] interval", {
  minimum <- (-5)
  maximum <- 5

  min_chromosome <- rep(FALSE, 16)
  expect_equal(binary_chromosome_to_numeric(min_chromosome, minimum, maximum), minimum)

  max_chromosome <- rep(TRUE, 16)
  expect_equal(binary_chromosome_to_numeric(max_chromosome, minimum, maximum), maximum)

  small_value_chromosome <- rep(FALSE, 16)
  small_value_chromosome[1] <- TRUE
  expect_gt(binary_chromosome_to_numeric(small_value_chromosome, minimum, maximum), minimum)
  expect_lt(binary_chromosome_to_numeric(small_value_chromosome, minimum, maximum), -4.9)

  high_value_chromosome <- rep(TRUE, 16)
  high_value_chromosome[1] <- FALSE
  expect_lt(binary_chromosome_to_numeric(high_value_chromosome, minimum, maximum), maximum)
  expect_gt(binary_chromosome_to_numeric(high_value_chromosome, minimum, maximum), 4.9)
})
