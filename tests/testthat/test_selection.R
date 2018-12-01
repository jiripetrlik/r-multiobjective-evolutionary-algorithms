library(testthat)
library(rmoea)

context("Selection tests")

test_that("Tournament selection test", {
  set.seed(1)
  size <- 10000
  better_fitness <- 1
  worse_fitness <- 100

  population_fitness <- c(rep(better_fitness, size / 2), rep(worse_fitness, size / 2))
  population_fitness <- sample(population_fitness)
  new_population_fitness <- population_fitness[tournament_selection(population_fitness)]

  expect_true(all(c(better_fitness, worse_fitness) %in% new_population_fitness))
  expect_true(sum(new_population_fitness == better_fitness) > sum(new_population_fitness == worse_fitness))
})
