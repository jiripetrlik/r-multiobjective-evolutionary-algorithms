library(testthat)
library(rmoea)

context("Pareto dominance tests")

test_that("Test Pareto dominance relation (minimize)", {
  a <- c(2, 4, 8)
  b <- c(5, 4, 8)
  expect_true(pareto_dominates(a, b))
  
  a <- c(5, 1, 1)
  b <- c(5, 4, 8)
  expect_true(pareto_dominates(a, b))
  
  a <- c(5, 4, 8)
  b <- c(5, 4, 8)
  expect_false(pareto_dominates(a, b))
  
  a <- c(5, 4, 8)
  b <- c(5, 1, 8)
  expect_false(pareto_dominates(a, b))
})

test_that("Test Pareto dominance relation (maximize)", {
  a <- c(2, 4, 8)
  b <- c(5, 4, 8)
  expect_false(pareto_dominates(a, b, minimize = FALSE))
  
  a <- c(5, 1, 1)
  b <- c(5, 4, 8)
  expect_false(pareto_dominates(a, b, minimize = FALSE))
  
  a <- c(5, 4, 8)
  b <- c(5, 4, 8)
  expect_false(pareto_dominates(a, b, minimize = FALSE))
  
  a <- c(5, 4, 8)
  b <- c(5, 1, 8)
  expect_true(pareto_dominates(a, b, minimize = FALSE))
})

test_that("Test convert_objective_matrix_to_list function", {
  m <- matrix(c(1,2,3,4,5,6), nrow = 2, byrow = T)
  l <- convert_objective_matrix_to_list(m)
  expect_length(l, 2)
  expect_length(l[[1]], 3)
  expect_length(l[[2]], 3)
  expect_true(all(l[[1]] == c(1, 2, 3)))
  expect_true(all(l[[2]] == c(4, 5, 6)))
})

test_that("Test check objective vectors list (positive scenario)", {
  l <- list()
  l[[1]] <- c(1, 5, 8, 3)
  l[[2]] <- c(4, 5, 8, 7)
  l[[3]] <- c(11, 5, 9, 2)
  
  expect_silent(check_objective_vectors_list(l))
})

test_that("Test check objective vectors list (non numeric vector)", {
  l <- list()
  l[[1]] <- c(1, 5, 8, 3)
  l[[2]] <- c("Hello")
  l[[3]] <- c(11, 5, 9, 2)
  
  expect_error(check_objective_vectors_list(l), "All items in objective vector list must be numeric vectors")
})

test_that("Test check objective vectors list (different length)", {
  l <- list()
  l[[1]] <- c(1, 5, 8, 3)
  l[[2]] <- c(4, 5, 8)
  l[[3]] <- c(11, 5, 9, 2)
  
  expect_error(check_objective_vectors_list(l), "All vectors in objective vector list must have the same size")
})

test_that("Test is non dominated (minimize)", {
  l <- list()
  l[[1]] <- c(5, 4, 3)
  l[[2]] <- c(3, 4, 5)
  l[[3]] <- c(4, 4, 4)
  expect_true(is_nondominated(c(2, 2, 1), l))
  expect_true(is_nondominated(c(5, 4, 3), l))
  expect_false(is_nondominated(c(7, 4, 3), l))
})

test_that("Test is non dominated (maximize)", {
  l <- list()
  l[[1]] <- c(5, 4, 3)
  l[[2]] <- c(3, 4, 5)
  l[[3]] <- c(4, 4, 4)
  expect_false(is_nondominated(c(2, 2, 2), l, minimize = FALSE))
  expect_true(is_nondominated(c(5, 4, 3), l, minimize = FALSE))
  expect_true(is_nondominated(c(7, 4, 3), l, minimize = FALSE))
})

test_that("Test is non dominated (bad vectors)", {
  l <- list()
  l[[1]] <- c(5, 4, 3)
  l[[2]] <- c(3, 5)
  l[[3]] <- c(4, 4, 4)
  expect_error(is_nondominated(c(2, 2, 2), l, minimize = FALSE), "All vectors in objective vector list must have the same size")
})

test_that("Test find non dominated (minimize)", {
  l <- list()
  l[[1]] <- c(5, 4, 3)
  l[[2]] <- c(8, 4, 3)
  l[[3]] <- c(3, 4, 5)
  l[[4]] <- c(4, 4, 4)
  l[[5]] <- c(4, 4, 9)
  expect_equal(find_nondominated(l), c(T, F, T, T, F))
})

test_that("Test find non dominated (maximize)", {
  l <- list()
  l[[1]] <- c(5, 4, 3)
  l[[2]] <- c(8, 4, 3)
  l[[3]] <- c(3, 4, 5)
  l[[4]] <- c(4, 4, 4)
  l[[5]] <- c(4, 4, 9)
  expect_equal(find_nondominated(l, minimize = FALSE), c(F, T, F, F, T))
})

test_that("Test find non dominated (bad vectors)", {
  l <- list()
  l[[1]] <- c(5, 4, 3)
  l[[2]] <- c("Hello")
  expect_error(find_nondominated(l), "All items in objective vector list must be numeric vectors")
})
