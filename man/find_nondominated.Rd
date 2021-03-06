% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pareto-dominance.R
\name{find_nondominated}
\alias{find_nondominated}
\title{Find nondominated solutions}
\usage{
find_nondominated(solutions, minimize = TRUE)
}
\arguments{
\item{solutions}{List or matrix with objective functions values for \code{solutions}.
In matrix each row contains values for one solution}

\item{minimize}{If objective functions should be minimized}
}
\value{
Logical vector with the same length as number of solutions. Vector contains
TRUE for solutions which are nondominated.
}
\description{
Find solutions which are not dominated by any other solution
}
\examples{
solutions <- list(c(1, 5, 5), c(8, 4, 8), c(3, 2, 2))
find_nondominated(solutions)

}
