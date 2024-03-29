% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dateutils.R
\name{stack_obs}
\alias{stack_obs}
\title{Stack time series observations in VAR format}
\usage{
stack_obs(Dat, p)
}
\arguments{
\item{Dat}{Data in a format convertable to a matrix}

\item{p}{number of lags, integer value}
}
\value{
stacked time series obs with p lags
}
\description{
Stack time series observations in VAR format over
series for p lags
}
\examples{
mat <- matrix(rnorm(100),50,2)
Z <- stack_obs(mat, 2) ## stack the dataset `mat` with two lags 
## Note: one "lag" will just return the original dataset.
}
