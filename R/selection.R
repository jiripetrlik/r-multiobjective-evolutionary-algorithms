tournament_selection <- function(population) {
  population_size <- length(population)
  random_choice_1 <- sample(population_size, replace = TRUE)
  random_choice_2 <- sample(population_size, replace = TRUE)

  new_population <- random_choice_1
  selection <- population[random_choice_1] > population[random_choice_2]
  new_population[selection] <- random_choice_2[selection]

  return(new_population)
}
