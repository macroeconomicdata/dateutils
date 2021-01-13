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