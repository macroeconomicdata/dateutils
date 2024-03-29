% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dateutils.R
\name{comp_form}
\alias{comp_form}
\title{Companion Form}
\usage{
comp_form(B)
}
\arguments{
\item{B}{Transition matrix from a VAR model}
}
\value{
Companion matrix of the input matrix
}
\description{
Put the transition matrix `B` into companion form
}
\examples{
comp_form(matrix(c(1:4), nrow = 2, byrow = TRUE)) ## matrix(c(4,-2,-3,1), nrow = 2, byrow = TRUE)
}
