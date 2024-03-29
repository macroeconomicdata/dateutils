% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dateutils.R
\name{end_of_period}
\alias{end_of_period}
\title{End of period date}
\usage{
end_of_period(dates, period = c("month", "week", "quarter", "year"), shift = 0)
}
\arguments{
\item{dates}{Date values formatted as.Date()}

\item{period}{One of `'month'`, `'week'`, `'quarter'`, `'year'`.}

\item{shift}{Integer, shift date forward (positive values) or backwards (negative values) by the number of periods.}
}
\value{
Last day of period in as.Date() format
}
\description{
Return the date of the last day of the period (week, month, quarter, year). Weekly dates are indexed to Friday.
}
\examples{
end_of_period(as.Date("2019-09-15")) ## 2019-09-30
}
