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
  expect_equal(result, c(1, 2, 2, 3, 1))
})
