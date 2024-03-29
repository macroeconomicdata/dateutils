% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{last_in_year}
\alias{last_in_year}
\title{Last date in the year}
\usage{
last_in_year(dates)
}
\arguments{
\item{dates}{A sequence of dates in `as.Date()` format}
}
\value{
Last day of the year
}
\description{
Return the latest date in each year for the values in `dates`
}
\examples{
dates <- seq.Date(from = as.Date("2020-09-11"),
                  by = "day", length.out = 10)
last_in_year(dates)
}
