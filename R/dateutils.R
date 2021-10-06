#' Number of days in a given month
#'
#' Get the number of days in a month given the year and month
#' 
#' @param year integer year value
#' @param month integer month value
#' @return The number of days in the month (integer)
#' @example 
#' month_days(2021,9) # 30
#' month_days(2020,2) # 29
month_days <- function(year, month) MonthDays(year, month)

#' End of period date
#'
#' Return the date of the last day of the period (week, month, quarter, year). Weekly dates are indexed to Friday.
#' 
#' @param dates Date values formatted as.Date()
#' @param period One of `'month'`, `'week'`, `'quarter'`, `'year'`.
#' @param shift Integer, shift date forward (positive values) or backwards (negative values) by the number of periods.
#' @return Last day of period in as.Date() format
#' @example 
#' end_of_period(as.Date("2019-09-15")) # 2019-09-30
end_of_period <- function(dates, period = c('month', 'week', 'quarter', 'year'), shift = 0){
  period <- match.arg(period)
  shift <- round(shift) # must be integer valued
  if(period == 'week') return(index_by_friday(dates) + 7*shift)
  else if(period == 'month') End_of_Month(dates, shift)
  else if(period == 'quarter') End_of_Quarter(dates, shift)
  else if(period == 'year') return(as.Date(paste(format(dates, "%Y") + shift, "12", "31", sep = "-")))
}

#' First of previous quarter date
#'
#' Return the date of the first day of the previous quarter
#' 
#' @param date date value formated as.Date()
#' @return The first day of the previous quarter of the date 
#' @example 
#' first_previous_quarter(as.Date("2019-09-15")) # 2019-04-01
first_previous_quarter <- function(date) First_previous_Quarter(date)


#' Return the day of a Date value
#'
#' Return the day of a Date value as an integer
#' 
#' @param date date value formated as.Date()
#' @return the day of the date (integer)
#' @example 
#' day(as.Date("2019-09-15")) # 15
day <- function(date) c(Day(date))

#' Stack time series observations in VAR format
#'
#' Stack time series observations in VAR format over
#' series for p lags
#' 
#' @param Dat Data in a format convertable to a matrix
#' @param p number of lags, integer value
#' @return stacked time series obs, lagged by p periods
#' @example 
#' stack_obs(data, 2) # stack the dataset `data`, lagging 2 periods
stack_obs <- function(Dat, p) Stack_Obs(Dat, p)

#' Companion Form
#'
#' Put the transition matrix `B` into companion form 
#' 
#' @param B Transition matrix from a VAR model
#' @return Companion matrix of the input matrix
#' @example 
#' comp_form(matrix(c(1:4), nrow = 2, byrow = TRUE)) # matrix(c(4,-2,-3,1), nrow = 2, byrow = TRUE)
comp_form <- function(B) Comp_Form(B)

#' End of Year
#'
#' Find the end of year for a vector of dates
#' 
#' @param dates Transition matrix from a VAR model
#' @return The last day of the year for the dates
#' @example 
#' end_of_year(as.Date("2019-09-15")) # 2019-12-31
end_of_year <- function(dates) End_Of_Year(dates)

#' Fill Forward
#'
#' Fill missing observations forward using the last finite observation
#' 
#' @param x Transition matrix from a VAR model
#' @return x with missing obs filled by forward value
#' @example 
fill_forward <- function(x) Fill_Forward(x)

#' First of Quarter
#'
#' Find the first date in the quarter for a vector of dates
#' 
#' @param dates Transition matrix from a VAR model
#' @return The first day of the quarter for the dates
#' @example 
#' first_of_quarter(as.Date("2019-9-15")) # 2019-07-01
first_of_quarter <- function(dates) First_Of_Quarter(dates)

#' Long Run Variance of a VAR
#'
#' Find the long run variance of a VAR using the transition equation `A` and shocks to observations `Q` 
#' 
#' @param A Transition matrix from a VAR model in companion form
#' @param Q Covariance of shocks
#' @param m Number of series in the VAR
#' @param p Number of lags in the VAR
#' @return The variance 
long_run_var <- function(A, Q, m, p) Long_Run_Var(A, Q, m, p)

#' Dummies for Numeric Data
#'
#' Create dummy variables for unique numeric values in `x`
#' 
#' @param x Numeric vector
#' @return Dummy variables for each unique value in the data
#' @example 
#' numdum(c(3,4,5)) # dummy1/2/3 = 1 if it is 3/4/5 and 0 otherwise
numdum <- function(x) NumDum(x)

#' Rolling Max
#'
#' Find the rolling maximum in `x` with span `n`
#' 
#' @param x Numeric vector
#' @param n Integer span
#' @return The maximum value of `x` with span `n`
#' @example 
#' rollmax(c(1,2,3), 2) # c(2,3,3)
rollmax <- function(x, n) RollMax(x, n)

#' Rolling Min
#'
#' Find the rolling minimum in `x` with span `n`
#' 
#' @param x Numeric vector
#' @param n Integer span
#' @return The minimum value of `x` with span `n`
#' @example 
#' rollmin(c(1,2,3),2) # 
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