% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dateutils.R
\name{numdum}
\alias{numdum}
\title{Dummies for Numeric Data}
\usage{
numdum(x)
}
\arguments{
\item{x}{Numeric vector}
}
\value{
Dummy variables for each unique value in the data
}
\description{
Create dummy variables for unique numeric values in `x`
}
\examples{
numdum(c(3,3,5,3,4,3,5,4,4,5)) ## dummies for each of 3, 4, and 5
}
