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
