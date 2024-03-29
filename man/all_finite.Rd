% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{all_finite}
\alias{all_finite}
\title{Rows with only finite values}
\usage{
all_finite(Y)
}
\arguments{
\item{Y}{matrix like data object}
}
\value{
Indexes of rows with with only finite values
}
\description{
Return indexes of rows with only finite values
}
\examples{
X <- matrix(1,10,2)
X[3,1] <- NA
all_finite(X)
}
