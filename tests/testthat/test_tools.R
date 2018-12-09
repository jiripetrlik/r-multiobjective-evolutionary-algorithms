library(testthat)
library(rmoea)

context("Tools tests")

test_that("Bind parameters test", {
  size <- 10
  value <- 5

  rep10times <- bind_parameters(rep.int, times = size)
  vector <- rep10times(value)
  expect_length(vector, size)
  expect_true(all(vector == value))
})

test_that("Test random integer", {
  set.seed(1)
  size <- 1000
  min <- (-5)
  max <- 5

  v <- random_integer(-5, 5, size)
  expect_true(is.integer(v))
  expect_length(v, size)
  expect_equal(sort(unique(v)), min:max)
})
