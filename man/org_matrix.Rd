% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/org_matrix.R
\name{org_matrix}
\alias{org_matrix}
\title{Organize rows and columns of a matrix}
\usage{
org_matrix(m, method = "alpha")
}
\arguments{
\item{m}{A matrix with individuals ordered identically in rows and columns.}

\item{method}{The method to be used to reorganize the matrix.
\code{method="alpha"} is the default and will organize rows/columns based
on alphanumeric order of rownames/colnames. \code{method="wins"} will
return a matrix ordered in descending order of summed rows (i.e. total
competitive interactions won). If rows have tied number of total wins,
they will be returned in the order of the inputted matrix.
 \code{method="ds"} Will return a matrix ordered by David's Score.}
}
\value{
The same matrix \code{m} with reordered rows/columns
}
\description{
Organize rows and columns of a matrix
}
\examples{
org_matrix(bonobos)
org_matrix(mouse, method="wins")
org_matrix(people, method="ds")
}

