% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seas_utils.R
\name{spline_fill_trend}
\alias{spline_fill_trend}
\title{Spline fill missing observations}
\usage{
spline_fill_trend(x)
}
\arguments{
\item{x}{data with missing observations}
}
\value{
data with interpolated missing observations
}
\description{
Spline fill missing observations, designed for filling low frequency trend estimates
}
\examples{
spline_fill_trend(c(1,2,3,NA,5)) ## 1 2 3 4 5
}
