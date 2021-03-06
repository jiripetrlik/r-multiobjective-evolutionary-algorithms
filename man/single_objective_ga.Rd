% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/single_objective_ga.R
\name{single_objective_ga}
\alias{single_objective_ga}
\title{Single objective genetic algorithm}
\usage{
single_objective_ga(objective_function, chromosome_size,
  chromosome_type = "binary", population_size = 100,
  number_of_iterations = 100, elitism = TRUE, nc = 2,
  mutation_probability = 0.05, uniform_mutation_sd = 0.01)
}
\arguments{
\item{objective_function}{Objective function}

\item{chromosome_size}{Size of chromosome which represents candidate solutions}

\item{chromosome_type}{Chromosome type ("binary" or "numeric")}

\item{population_size}{Number of solutions evaluated in one iteration of genetic algorithm}

\item{number_of_iterations}{Number of iterations (generations) of genetic algorithm}

\item{elitism}{Use elitism}

\item{nc}{NC for SBX crossover (valid if "numeric" chromosome is used)}

\item{mutation_probability}{Probability of mutation (valid if "binary" chromosome is used)}

\item{uniform_mutation_sd}{Standard deviation of mutation (valid if "numeric" chromosome is used)}
}
\value{
List which contains results of single objective genetic algorithm:

\code{value} - Value of objective function for the best solution

\code{best_solution} - Chromosome which represents the best solution

\code{best_solution_index} - Index of the best solution in population

\code{statistics} - Statistics about run of genetic algorithm

\code{parameters} - Parameters of genetic algorithm
}
\description{
Use single objective genetic algorithm to find an optimum for
the specified objective function. Candidate solutions are represented
as logical or numeric vectors.
}
