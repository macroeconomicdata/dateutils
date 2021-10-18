#' Extract numeric values
#'
#' Extract numeric values from x
#' 
#' @param x object containing numeric (and other) values
#' @return Numeric values from the object
#' @example 
#' x <- c(1,2,3,"one","two","three")
#' extract_numeric(x) # 1,2,3,NA,NA,NA
extract_numeric <- function(x) as.numeric(gsub("[^0-9.-]+", "", as.character(x)))

#' Extract character values
#'
#' Extract character values from x including space and underscore
#' 
#' @param x object containing character values
#' @return Character valus from the object
#' @example 
#' x <- c(1,2,3,"one","two","three")
#' extract_character(x) # ""      ""      ""      "one"   "two"   "three"
extract_character <- function(x) trimws(gsub("([^A-Za-z _]|NA)","",as.character(x)))

#' Limit Characters
#'
#' limit the number of characters in a string and remove spacial characters (will not drop numbers)
#' 
#' @param x object containing character values
#' @param limit maximum number of characters to return
#' @return Character values within the limit
#' @example 
#' x <- c(1,2,3,"one","two","three")
#' limit_character(x,limit = 3)  # "1"   "2"   "3"   "one" "two" "thr" "TRU"
limit_character <- function(x, limit = 100) substr(gsub("([^A-Za-z0-9 _]|NA)","",as.character(x)), 1, limit)


#' Extract characters
#'
#' Extract character values from x excluding space and underscore
#' 
#' @param x object containing character (and other) values
#' @return Character valus without space and underscore
#' @example 
#' x <- c(1,2,3,"one","two","three")
#' extract_basic_character(x)  # ""      ""      ""      "one"   "two"   "three" "true" 
extract_basic_character <- function(x) tolower(trimws(gsub("([^A-Za-z]|NA)","",as.character(x))))

#' Rows with finite values
#'
#' Return indexes of rows with at least one finite value
#' 
#' @param Y matrix like data object
#' @return Indexes of rows with at least one finite value
#' @example 
#' any_finite(fred[1:10])
any_finite <- function(Y) seq(NROW(Y))%in%(any_obs_cols(t(as.matrix(Y)))+1)

#' Rows with only finite values
#'
#' Return indexes of rows with only finite values
#' 
#' @param Y matrix like data object
#' @return Indexes of rows with with only finite values
#' @example 
#' all_finite(fred[1:10])
all_finite <- function(Y) seq(NROW(Y))%in%(finite_cols(t(as.matrix(Y)))+1)

#' Number of finite values in a row
#'
#' Return the number of finite values in a row of Y
#' 
#' @param Y matrix like data object
#' @return The number of finite values
#' @example 
#' number_finite(fred[1:10])
number_finite <- function(Y) seq(NROW(Y))%in%(count_finite(t(as.matrix(Y)))+1)

#' Get from list
#'
#' Retrieve object `what` from `lst`
#' 
#' @param lst list
#' @param what object to retrieve (by name or index)
#' @return
#' @example 
get_from_list <- function(lst, what) lst[[what]]

#' Convert rows to list
#'
#' Return `Y` with each row as a list
#' 
#' @param Y matrix like data object
#' @return Each row as a list
#' @example 
#' row_to_list(fred[1:10])
row_to_list <- function(Y) split(Y, row(Y))

#' Convert columns to list
#'
#' Return `Y` with each column as a list
#' 
#' @param Y matrix like data object
#' @return Each column as a list
#' @example 
#' col_to_list(fred[1:10])
col_to_list <- function(Y) split(Y, col(Y))

#' Normalize daily dates in a data.table
#'
#' Normalize daily dates in a data.table to prevent missing days
#' 
#' @param DT a data.table
#' @param date_var name of date column
#' @return Daily dates without missing
#' @example 
#' fill_daily_dates(fred[series_name == "gdp constant prices"], "ref_date")
fill_daily_dates <- function(DT, date_var = "ref_date"){
  DT <- data.table(DT)
  setnames(DT, date_var, "ref_date")
  DT <- merge(data.table("ref_date" = seq.Date(min(DT$ref_date), max(DT$ref_date), by = "day")), DT, by = "ref_date", all = TRUE)
  return(DT)
}

#' Last observation
#'
#' Return the last finite observation of `x`
#' 
#' @param x data potentially with non-finite values
#' @return The last finite observation
#' last_obs(fred[series_name == "gdp constant prices", value])
last_obs <- function(x){
  idx <- which(is.finite(x))
  if(length(idx) == 0){
    return(NA)
  }else{
    return(x[max(idx)])
  }
} 

#' Percent change
#'
#' Calculate the percent change in `y` from one period to the next
#' 
#' @param y data 
#' @param lag number of periods for percent change
#' @return The percentage change among the lag period
#' @example 
#' pct_chng(fred[series_name == "gdp constant prices", value])
pct_chng <- function(y, lag = 1){
  y <- as.matrix(y)
  y <- (y[-seq(lag), ,drop = FALSE] - y[-seq(NROW(y)-lag+1, NROW(y)), ,drop = FALSE])/y[-seq(NROW(y)-lag+1, NROW(y)), ,drop = FALSE]
  y <- rbind(matrix(NA, lag, NCOL(y)), y) # keep the number of rows the same
  return(y)
}

#' Difference data
#'
#' Wrapper for `diff()` maintaining the same number of observations in `x`
#' 
#' @param x data
#' @param lag number of lags to use
#' @return Differenced data
#' @example 
#' Diff(fred[series_name == "gdp constant prices", value],lag = 1)
Diff <- function(x, lag = 1){ # difference but keep same number of rows
  if(is.null(dim(x))){
    out <- c(rep(NA, lag), diff(x, lag = lag))
  }else{
    out <- rbind(matrix(NA, lag, NCOL(x)), diff(x, lag = lag))
  }
  return(out)
}

index_one_by_friday <- function(dte){
  dte <- as.Date(dte)
  tmp_sq <- seq.Date(from = dte, to = dte+6, by = "day")
  idx <- which(weekdays(tmp_sq) == "Sunday")
  return(tmp_sq[idx] - 2)
}

#' Find the Friday in a given week
#'
#' Find the Friday in a given week from a sequence of Dates
#' Vectors should be in as.Date() format
#'
#' @param dates vector of dates
#' @return The date of the Friday in the week of the given date
#' @examples
#'
#' dates <- seq.Date(from = as.Date("2020-09-21"),
#'                   by = "week", length.out = 10)
#' fridays <- index_by_friday(dates)
#' weekdays(fridays)
index_by_friday <- function(dates){
  out <- lapply(dates, FUN = index_one_by_friday)
  return(do.call("c", out))
}

#' Return the mean
#'
#' Return the mean of `x`. If no observations, return `NA`. This is a workaround for the fact that in data.table, `:= mean(x, na.rm = TRUE)` will return `NaN` where there are no observations
#' 
#' @param x data potentially with non-finite values
mean_na <- function(x) if(all(!is.finite(x))) return(as.double(NA)) else return(as.double(mean(x, na.rm = T)))

#' Return the standard deviation
#'
#' Return the standard deviation of `x`. If no observations, return `NA`. This is a workaround for the fact that in data.table, `:= sd(x, na.rm = TRUE)` will return `NaN` where there are no observations
#' 
#' @param x data potentially with non-finite values
sd_na <- function(x) if(all(!is.finite(x))) return(as.double(NA)) else return(as.double(sd(x, na.rm = T)))

#' Return the sum
#'
#' Return the sum of `x`. If no observations, return `NA`. This is a workaround for the fact that in data.table, `:= sum()` will return `NaN` where there are no observations
#' 
#' @param x data potentially with non-finite values
sum_na <- function(x) if(all(!is.finite(x))) return(as.double(NA)) else return(as.double(sum(x, na.rm = T)))


#' Aggregate long format data.table
#'
#' Aggregate a data.table in long format to a specified frequency
#' 
#' @param dt_long data.table in long format
#' @param frq frequency for aggregation, one of `"month"`, `"week"`, `"quarter"`, or `"year"`
#' @param date_name name of date column
#' @param id_name name of id column
#' @param value_name name of value column
agg_to_freq <- function(dt_long, frq = c("month", "week", "quarter", "year"),
                        date_name = "ref_date", id_name = "series_name", value_name = "value"){
                        
  frq <- match.arg(frq) # checks and picks the first if unspecified
  dt_long <- data.table(dt_long)
  setnames(dt_long, date_name, "ref_date")
  if(!is.null(id_name)){
    setnames(dt_long, id_name, "series_name")
  }else{
    dt_long[ , series_name := "temp"]
  }
  setnames(dt_long, value_name, "value")
  if(frq == "week"){
    dt_out <- dt_long[ , mean_na(value),
                       by = .(series_name, index_by_friday(ref_date))]
    nobs_out <- dt_long[ , sum(is.finite(value)),
                      by = .(series_name, index_by_friday(ref_date))]
    setnames(dt_out, "V1", "value")
    setnames(dt_out, "index_by_friday", "ref_date")
    setnames(nobs_out, "V1", "n_obs")
    setnames(nobs_out, "index_by_friday", "ref_date")
    dt_out <- merge(dt_out, nobs_out, by = c("ref_date", "series_name"))
  }else if(frq == "month"){
    dt_out <- dt_long[ , mean_na(value),
                       by = .(series_name, end_of_month(ref_date))]
    nobs_out <- dt_long[ , sum(is.finite(value)),
                       by = .(series_name, end_of_month(ref_date))]
    setnames(dt_out, "V1", "value")
    setnames(dt_out, "end_of_month", "ref_date")
    setnames(nobs_out, "V1", "n_obs")
    setnames(nobs_out, "end_of_month", "ref_date")
    dt_out <- merge(dt_out, nobs_out, by = c("ref_date", "series_name"))
  }else if(frq == "quarter"){
    dt_out <- dt_long[ , mean_na(value),
                       by = .(series_name, end_of_quarter(ref_date))]
    nobs_out <- dt_long[ , sum(is.finite(value)),
                       by = .(series_name, end_of_quarter(ref_date))]
    setnames(dt_out, "V1", "value")
    setnames(dt_out, "end_of_quarter", "ref_date")
    setnames(nobs_out, "V1", "n_obs")
    setnames(nobs_out, "end_of_quarter", "ref_date")
    dt_out <- merge(dt_out, nobs_out, by = c("ref_date", "series_name"))
  }else if(frq == "year"){
    dt_out <- dt_long[ , mean_na(value),
                       by = .(series_name, end_of_year(ref_date))]
    nobs_out <- dt_long[ , sum(is.finite(value)),
                       by = .(series_name, end_of_year(ref_date))]
    setnames(dt_out, "V1", "value")
    setnames(dt_out, "end_of_year", "ref_date")
    setnames(nobs_out, "V1", "n_obs")
    setnames(nobs_out, "end_of_year", "ref_date")
    dt_out <- merge(dt_out, nobs_out, by = c("ref_date", "series_name"))
  }
  dt_out <- dt_out[n_obs >= 1] #drop missing obs in long format
  dt_out <- dt_out[order(series_name, ref_date)]
  if(is.null(id_name)) dt_out[ , series_name := NULL]
  return(dt_out)
}


#' Add NA values to the tail of a wide data.table
#'
#' Add NA values to the tail of a wide data.table to be filled by forecasting routines
#' 
#' @param dt data.table in wide format
#' @param horizon number of periods to add at specified `frq`
#' @param frq frequency for aggregation, one of `"month"`, `"week"`, `"quarter"`, or `"year"`
#' @param date_name name of date column
add_forecast_dates <- function(dt, horizon = 1, frq = c("month", "week", "quarter", "year"), date_name = "ref_date"){
  frq <- match.arg(frq)
  setnames(dt, date_name, "ref_date")
  if(frq == "day"){
    tmp <- seq.Date(from = max(dt$ref_date), length.out = 1 + horizon, by = "day")[-1]
    out <- rbind(dt, data.table("ref_date" = tmp), fill = TRUE)
  }else if(frq == "week"){
    tmp <- seq.Date(from = max(dt$ref_date), length.out = 1 + horizon, by = "week")[-1]
    out <- rbind(dt, data.table("ref_date" = tmp), fill = TRUE)
  }else if(frq == "month"){
    tmp <- end_of_month(seq.Date(from = first_of_month(max(dt$ref_date)), length.out = 1 + horizon, by = "month"))[-1]
    out <- rbind(dt, data.table("ref_date" = tmp), fill = TRUE)
  }else if(frq == "quarter"){
    tmp <- end_of_quarter(seq.Date(from = first_of_quarter(max(dt$ref_date)), length.out = 1 + horizon, by = "quarter"))[-1]
    out <- rbind(dt, data.table("ref_date" = tmp), fill = TRUE)
  }else if(frq == "year"){
    tmp <- seq.Date(from = max(dt$ref_date), length.out = 1 + horizon, by = "year")[-1]
    out <- rbind(dt, data.table("ref_date" = tmp), fill = TRUE)
  }else{
    stop("'frq' must be one of 'day', 'week', 'month', 'quarter', or 'year'")
  }
  return(out)
}


#' Count observations
#'
#' Return the number of finite observations in `x`
#' 
#' @param x data vector
count_obs <- function(x) as.integer(sum(is.finite(x)))

#' Last date in the week
#'
#' Return the latest date in each week for the values in `dates`
#' 
#' @param dates A sequence of dates in `as.Date()` format
last_in_week <- function(dates){
  dates <- data.table("ref_date" = dates)
  out <- dates[ , max(ref_date), by = index_by_friday(ref_date)]
  return(out$V1)
}

#' Last date in the month
#'
#' Return the latest date in each month for the values in `dates`
#' 
#' @param dates A sequence of dates in `as.Date()` format
last_in_month <- function(dates){
  dates <- data.table("ref_date" = dates)
  out <- dates[ , max(ref_date), by = end_of_month(ref_date)]
  return(out$V1)
}

#' First of month
#'
#' Return the first day of the month for each date in `dates`
#' 
#' @param dates A sequence of dates in `as.Date()` format
first_of_month <- function(dates) as.Date(paste(year(dates), month(dates), 01, sep = "-"))

#' Last date in the week
#'
#' Return the latest date in the quarter fop the values in `dates`
#' 
#' @param dates A sequence of dates in `as.Date()` format
last_in_quarter <- function(dates){
  dates <- data.table("ref_date" = dates)
  out <- dates[ , max(ref_date), by = end_of_quarter(ref_date)]
  return(out$V1)
}

#' Last date in the year
#'
#' Return the latest date in each year for the values in `dates`
#' 
#' @param dates A sequence of dates in `as.Date()` format
last_in_year <- function(dates){
  dates <- data.table("ref_date" = dates)
  out <- dates[ , max(ref_date), by = year(ref_date)]
  return(out$V1)
}




#' Aggregate data.table and return wide format
#'
#' Aggregate a data.table to a specified frequency and return wide format data
#' 
#' @param dt data.table in long format
#' @param frq frequency for aggregation, one of `"month"`, `"week"`, `"quarter"`, or `"year"`
#' @param date_name name of date column
#' @param id_name name of id column
#' @param value_name name of value column
#' @param dt_is_wide T/F, is input data `dt` in wide format
agg_to_freq_wide <- function(dt, date_name = "ref_date",  frq = c("month", "week", "quarter", "year"), id_name = "series_name",
                             value_name = "value", dt_is_wide = FALSE){
  frq <- match.arg(frq)
  setnames(dt, date_name, "ref_date")
  setnames(dt, id_name, "series_name")
  setnames(dt, value_name, "value")
  if(dt_is_wide){
    dt <- melt(dt, id.vars = 'ref_date', variable.name = "series_name", na.rm = TRUE)
  }
  dt <- agg_to_freq(dt, frq = frq)
  n_obs <- dcast(dt, ref_date ~ series_name, value.var = "n_obs", fun.aggregate = mean_na)
  dt <- dcast(dt, ref_date ~ series_name, value.var = "value", fun.aggregate = mean_na)
  if(frq == "week"){
    tmp_dt <- data.table("ref_date" = seq.Date(from = min(dt$ref_date), to = max(dt$ref_date), by = 7))# regularize frequency
  }else if(frq == "month"){
    tmp_dt <- data.table("ref_date" = end_of_month(seq.Date(from = first_of_month(min(dt$ref_date)),
                                                            to = first_of_month(max(dt$ref_date)), by = "month")))
  }else if(frq == "quarter"){
    tmp_dt <- data.table("ref_date" = end_of_quarter(seq.Date(from = first_of_month(min(dt$ref_date)),
                                                              to = first_of_month(max(dt$ref_date)), by = "quarter")))
  }else if(frq == "year"){
    tmp_dt <- data.table("ref_date" = seq.Date(from = min(dt$ref_date),
                                               to = max(dt$ref_date), by = "year"))
  }
  dt <- merge(tmp_dt, dt, by = "ref_date", all = TRUE) # regularize frequency
  n_obs <- merge(tmp_dt, n_obs, by = "ref_date", all = TRUE) # regularize frequency
  return(list(dt = dt,
              n_obs = n_obs))
}

#' Are all elements `NA`?
#'
#' Return a logical indicating if all elements are `NA`
#' 
#' @param x data vector
allNA <- function(x) all(is.na(x))

#' Find element of this_in that
#'
#' Find element of this_in that, ie `this_in%in%that`
#' 
#' @param that first object
#' @param this_in second object
is_in <- function(that, this_in) this_in%in%that


#' Rolling mean
#'
#' Take the rolling mean of `x` over `n` elements
#' 
#' @param x data vector
#' @param n span of rolling mean
rollmean <- function(x, n){
  y <- rollmean_cpp(x,n)
  y[!is.finite(y)] <- NA
  return(y)
}

#' Get frequency of data based on missing observations
#'
#' Guess the frequency of a data series based on the pattern of missing observations
#'
#' @param x data, potentially with missing observations
#' @param dates corresponding dates in `as.Date()` format
get_data_frq <- function(x = NULL, dates){
  if(is.null(x)){
    ddates <- median(diff(dates))
  }else{
    ddates <- median(diff(dates[!is.na(x)]))
  }
  if(ddates > 300){
    return("year")
  }else if(ddates > 89 && ddates < 93){
    return("quarter")
  }else if(ddates > 26 && ddates < 32){
    return("month")
  }else if(ddates == 7){
    return("week")
  }else if(ddates == 1){
    return("day")
  }else{
    return(NA)
  }
}


#' Format weekly frequency data
#'
#' Regularlize data to weekly frequency so that all observations fall at the end of the week
#' 
#' @param dt wide format data.table
#' @param date_name name of column containing dates in `as.Date()` format
format_weekly <- function(dt, date_name = "ref_date"){
  dt <- data.table(dt)
  allnames <- names(dt)[-1]
  setnames(dt, date_name, "ref_date")
  setcolorder(dt, "ref_date")
  frqs <- sapply(dt[ , -1, with = FALSE], FUN = get_data_frq, dates = dt$ref_date)
  varnames <- allnames[frqs=="week"]
  if(length(varnames) == 0) stop("No weekly data")
  dt_tmp <- agg_to_freq_wide(dt[ , c("ref_date", varnames), with = FALSE], frq = "week")
  dt <- dt[ , -varnames, with = FALSE]
  dt <- merge(dt, dt_tmp, by = "ref_date", all = TRUE)
  setcolorder(dt, c("ref_date", allnames))
  return(dt)
}



