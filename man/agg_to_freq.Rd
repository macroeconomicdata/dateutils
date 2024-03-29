% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{agg_to_freq}
\alias{agg_to_freq}
\title{Aggregate long format data.table}
\usage{
agg_to_freq(
  dt_long,
  frq = c("month", "week", "quarter", "year"),
  date_name = "ref_date",
  id_name = "series_name",
  value_name = "value"
)
}
\arguments{
\item{dt_long}{data.table in long format}

\item{frq}{frequency for aggregation, one of `"month"`, `"week"`, `"quarter"`, or `"year"`}

\item{date_name}{name of date column}

\item{id_name}{name of id column}

\item{value_name}{name of value column}
}
\value{
Aggregated data at specified frequency in long format
}
\description{
Aggregate a data.table in long format to a specified frequency
}
\examples{
out <- agg_to_freq(fred[series_name == "gdp constant prices"], frq = "year")
}
