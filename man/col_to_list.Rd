% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{col_to_list}
\alias{col_to_list}
\title{Convert columns to list}
\usage{
col_to_list(Y)
}
\arguments{
\item{Y}{matrix like data object}
}
\value{
Each column as a list
}
\description{
Return `Y` with each column as a list
}
\examples{
row_to_list(matrix(rnorm(20),10,2))
}
