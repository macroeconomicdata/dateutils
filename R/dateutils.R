#' Number of days in a given month
#'
#' Get the number of days in a month given the year and month
#' 
#' @param year integer year value
#' @param month integer month value
month_days <- function(year, month) MonthDays(year, month)

#' End of month date
#'
#' Return the date of the last day of the month
#' 
#' @param date date value formated as.Date()
end_of_month <- function(date) End_of_Month(date)

#' End of next month date
#'
#' Return the date of the last day of the following month
#' 
#' @param date date value formated as.Date()
end_next_month <- function(date) End_next_Month(date)

#' End of previous month date
#'
#' Return the date of the last day of the previous month
#' 
#' @param date date value formated as.Date()
end_previous_month <- function(date) End_previous_Month(date)

#' End of quarter date
#'
#' Return the date of the last day of the quarter
#' 
#' @param date date value formated as.Date()
end_of_quarter <- function(date) End_of_Quarter(date)

#' End of previous quarter date
#'
#' Return the date of the last day of the previous quarter
#' 
#' @param date date value formated as.Date()
end_previous_quarter <- function(date) End_previous_Quarter(date)

#' First of previous quarter date
#'
#' Return the date of the first day of the previous quarter
#' 
#' @param date date value formated as.Date()
first_previous_quarter <- function(date) First_previous_Quarter(date)


#' Return the day of a Date value
#'
#' Return the day of a Date value as an integer
#' 
#' @param date date value formated as.Date()
day <- function(date) c(Day(date))

#' Stack time series observations in VAR format
#'
#' Stack time series observations in VAR format over
#' series for p lags
#' 
#' @param Dat Data in a format convertable to a matrix
#' @param p number of lags, integer value
stack_obs <- function(Dat, p) Stack_Obs(Dat, p)

#' Companion Form
#'
#' Put the transition matrix `B` into companion form 
#' 
#' @param B Transition matrix from a VAR model
comp_form <- function(B) Comp_Form(B)

#' End of Year
#'
#' Find the end of year for a vector of dates
#' 
#' @param dates Transition matrix from a VAR model
end_of_year <- function(dates) End_Of_Year(dates)

#' Fill Forward
#'
#' Fill missing observations forward using the last finite observation
#' 
#' @param x Transition matrix from a VAR model
fill_forward <- function(x) Fill_Forward(x)

#' First of Quarter
#'
#' Find the first date in the quarter for a vector of dates
#' 
#' @param dates Transition matrix from a VAR model
first_of_quarter <- function(dates) First_Of_Quarter(dates)

#' Long Run Variance of a VAR
#'
#' Find the long run variance of a VAR using the transition equation `A` and shocks to observations `Q` 
#' 
#' @param A Transition matrix from a VAR model in companion form
#' @param Q Covariance of shocks
#' @param m Number of series in the VAR
#' @param p Number of lags in the VAR
long_run_var <- function(A, Q, m, p) Long_Run_Var(A, Q, m, p)

#' Dummies for Numeric Data
#'
#' Create dummy variables for unique numeric values in `x`
#' 
#' @param x Numeric vector
numdum <- function(x) NumDum(x)

#' Rolling Max
#'
#' Find the rolling maximum in `x` with span `n`
#' 
#' @param x Numeric vector
#' @param n Integer span
rollmax <- function(x, n) RollMax(x, n)

#' Rolling Min
#'
#' Find the rolling minimum in `x` with span `n`
#' 
#' @param x Numeric vector
#' @param n Integer span
rollmin <- function(x, n) RollMin(x, n)

#' Sample mixed frequency data from FRED
#'
#' @name fred
#' @docType data
#' @author Seth Leonard \email{seth@@macroeconomicdata.com}
#' @references \url{https://fred.stlouisfed.org/}
#' @keywords data
NULL

#' Library of metadata for mixed frequency dataset `fred`
#'
#' @name fredlib
#' @docType data
#' @author Seth Leonard \email{seth@@macroeconomicdata.com}
#' @references \url{https://fred.stlouisfed.org/}
#' @keywords data
NULL