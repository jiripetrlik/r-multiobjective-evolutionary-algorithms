library(testthat)
library(rmoea)

context("Single objective genetic algorithm tests")

test_that("Test binary chromosome GA using quadratic benchmark", {
  set.seed(1)

  objective_function <- function(chromosome) {
    x1 <- binary_chromosome_to_numeric(chromosome[1:16], -10, 10)
    x2 <- binary_chromosome_to_numeric(chromosome[17:32], -10, 10)

    return(quadratic_benchmark_function(c(x1 - 1, x2 + 1)))
  }

  results <- single_objective_ga(objective_function, "binary", nBits = 32)
  expect_true(is.list(results))
  expect_lt(results$value, 0.01)
  expect_lt(abs(binary_chromosome_to_numeric(results$best_solution[1:16], -10, 10) - 1), 0.1)
  expect_lt(abs(binary_chromosome_to_numeric(results$best_solution[17:32], -10, 10) + 1), 0.1)

  expect_length(results$statistics$min_fitness, 100)
  expect_length(results$statistics$max_fitness, 100)
  expect_length(results$statistics$mean_fitness, 100)
  expect_length(results$statistics$sd_fitness, 100)
})

test_that("Test numeric chromosome GA using quadratic benchmark", {
  set.seed(1)

  objective_function <- function(chromosome) {
    x1 <- chromosome[1]
    x2 <- chromosome[2]

    return(quadratic_benchmark_function(c(x1 - 1, x2 + 1)))
  }

  results <- single_objective_ga(objective_function, "real-valued", lower = c(-10, -10), upper = c(10, 10))
  expect_true(is.list(results))
  expect_lt(results$value, 0.01)
  expect_lt(abs(results$best_solution[1] - 1), 0.1)
  expect_lt(abs(results$best_solution[2] + 1), 0.1)

  expect_length(results$statistics$min_fitness, 100)
  expect_length(results$statistics$max_fitness, 100)
  expect_length(results$statistics$mean_fitness, 100)
  expect_length(results$statistics$sd_fitness, 100)
})

test_that("Test binary single objective GA parameters", {
  objective_function <- function(chromosome) {
    x1 <- binary_chromosome_to_numeric(chromosome[1:16], -10, 10)
    x2 <- binary_chromosome_to_numeric(chromosome[17:32], -10, 10)

    return(quadratic_benchmark_function(c(x1 - 1, x2 + 1)))
  }
  nBits <- 32
  chromosome_type <- "binary"
  population_size <- 50
  number_of_iterations <- 10
  elitism <- FALSE
  mutation_probability <- 0.1

  results <- single_objective_ga(objective_function,
                                 chromosome_type = chromosome_type,
                                 nBits = nBits,
                                 population_size = population_size,
                                 number_of_iterations = number_of_iterations,
                                 elitism = elitism,
                                 mutation_probability = mutation_probability);

  expect_equal(results$parameters$nBits, nBits)
  expect_equal(results$parameters$chromosome_type, chromosome_type)
  expect_equal(results$parameters$population_size, population_size)
  expect_equal(results$parameters$number_of_iterations, number_of_iterations)
  expect_equal(results$parameters$elitism, elitism)
  expect_equal(results$parameters$mutation_probability, mutation_probability)
})

test_that("Test numeric single objective GA parameters", {
  objective_function <- function(chromosome) {
    chromosome <- scale_numeric_chromosome(chromosome, -10, 10)
    x1 <- chromosome[1]
    x2 <- chromosome[2]

    return(quadratic_benchmark_function(c(x1 - 1, x2 + 1)))
  }
  chromosome_size <- 2
  lower <- c(0, 0)
  upper <- c(1, 1)
  chromosome_type <- "real-valued"
  population_size <- 50
  number_of_iterations <- 10
  elitism <- FALSE
  nc <- 5
  uniform_mutation_sd <- 0.1

  results <- single_objective_ga(objective_function,
                                 chromosome_type = chromosome_type,
                                 lower = lower,
                                 upper = upper,
                                 population_size = population_size,
                                 number_of_iterations = number_of_iterations,
                                 elitism = elitism,
                                 nc = nc,
                                 uniform_mutation_sd = uniform_mutation_sd)

  expect_equal(results$parameters$lower, lower)
  expect_equal(results$parameters$upper, upper)
  expect_equal(results$parameters$chromosome_type, chromosome_type)
  expect_equal(results$parameters$population_size, population_size)
  expect_equal(results$parameters$number_of_iterations, number_of_iterations)
  expect_equal(results$parameters$elitism, elitism)
  expect_equal(results$parameters$nc, nc)
  expect_equal(results$parameters$uniform_mutation_sd, uniform_mutation_sd)
})
