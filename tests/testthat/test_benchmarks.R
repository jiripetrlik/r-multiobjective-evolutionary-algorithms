library(testthat)
library(rmoea)

context("Benchmarks tests")

test_that("Quadratic benchmark test", {
  expect_equal(quadratic_benchmark_function(c(0, 0)), 0)
  expect_equal(quadratic_benchmark_function(c(5, -2)), 29)
})
