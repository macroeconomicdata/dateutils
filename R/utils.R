#library(data.table)

any_finite <- function(Y) seq(NROW(Y))%in%(any_obs_cols(t(as.matrix(Y)))+1)
all_finite <- function(Y) seq(NROW(Y))%in%(finite_cols(t(as.matrix(Y)))+1)
number_finite <- function(Y) seq(NROW(Y))%in%(count_finite(t(as.matrix(Y)))+1)

get_from_list <- function(lst, what) lst[[what]]
row_to_list <- function(Y) split(Y, row(Y))
col_to_list <- function(Y) split(Y, col(Y))

fill_daily_dates <- function(DT){
  DT <- merge(data.table("ref_date" = seq.Date(min(DT$ref_date), max(DT$ref_date), by = "day")), DT, by = "ref_date", all = TRUE)
  return(DT)
}

last_obs <- function(x){
  idx <- which(is.finite(x))
  if(length(idx) == 0){
    return(NA)
  }else{
    return(x[max(idx)])
  }
} 

pct_chng <- function(y, lag = 1){
  y <- as.matrix(y)
  y <- (y[-seq(lag), ,drop = FALSE] - y[-seq(NROW(y)-lag+1, NROW(y)), ,drop = FALSE])/y[-seq(NROW(y)-lag+1, NROW(y)), ,drop = FALSE]
  y <- rbind(matrix(NA, lag, NCOL(y)), y) # keep the number of rows the same
  return(y)
}

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
#' weekdays(friday)
index_by_friday <- function(dates){
  out <- lapply(dates, FUN = index_one_by_friday)
  return(do.call("c", out))
}

mean_na <- function(x) if(all(!is.finite(x))) return(as.double(NA)) else return(as.double(mean(x, na.rm = T)))

sum_na <- function(x) if(all(!is.finite(x))) return(as.double(NA)) else return(as.double(sum(x, na.rm = T)))



agg_to_freq <- function(dt_long, date_name = "ref_date",
                         id_name = "series_name", value_name = "value",
                         frq = "month"){
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
  }else{
    stop("'frq' must be one of 'week', 'month', 'quarter', or 'year'")
  }
  dt_out <- dt_out[n_obs >= 1] #drop missing obs in long format
  dt_out <- dt_out[order(series_name, ref_date)]
  if(is.null(id_name)) dt_out[ , series_name := NULL]
  return(dt_out)
}



add_forecast_dates <- function(dt, horizon = 1, date_name = "ref_date", frq = "month"){
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

get_data_frq <- function(x, dates){
  ddates <- median(diff(dates[!is.na(x)]))
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

backfill <- function(x, dates){
  frq <- get_data_frq(x, dates)
  X <- data.table("ref_date" = dates, "value" = x)
  if(frq == "year"){
    X[ , backfill := mean_na(value), by = year(ref_date)]
  }else if(frq == "quarter"){
    X[ , backfill := mean_na(value), by = end_of_quarter(ref_date)]
  }else if(frq == "month"){
    X[ , backfill := mean_na(value), by = end_of_month(ref_date)]
  }else if(frq == "week"){
    X[ , backfill := mean_na(value), by = index_by_friday(ref_date)]
  }else{
    return(x)
  }
  return(X$backfill)
}

cummean <- function(x, n_obs){
  if(all(!is.finite(x))){
    return(x)
  }else{
    is_finite <- is.finite(x)
    n_obs <- sum(is_finite)
    obs <- min(which(is_finite)):length(x)
    x[obs[!obs%in%which(is_finite)]] <- 0
    x[obs] <- cumsum(x[obs])/mean(n_obs, na.rm = TRUE)
    return(x)
  }
}


count_obs <- function(x) as.integer(sum(is.finite(x)))
   

# count_obs <- function(x){
#   if(all(!is.finite(x))){
#     return(as.integer(NA))
#   }else{
#     return(as.integer(sum(is.finite(x))))
#   }
# }

cummean_fill <- function(x, dates, frq = "month"){
  X <- data.table("ref_date" = dates, "value" = x)
  if(frq == "year"){
    #X[ , obs := count_obs(value),  by = year(ref_date)]
    X[ , backfill := cummean(value), by = year(ref_date)]
  }else if(frq == "quarter"){
    #X[ , obs := count_obs(value),  by = end_of_quarter(ref_date)]
    X[ , backfill := cummean(value), by = end_of_quarter(ref_date)]
  }else if(frq == "month"){
    #X[ , obs := count_obs(value),  by = end_of_month(ref_date)]
    X[ , backfill := cummean(value), by = end_of_month(ref_date)]
  }else if(frq == "week"){
    #X[ , obs := count_obs(value),  by = index_by_friday(ref_date)]
    X[ , backfill := cummean(value), by = index_by_friday(ref_date)]
  }else{
    return(x)
  }
  return(X$backfill)
}

last_in_week <- function(dates){
  dates <- data.table("ref_date" = dates)
  out <- dates[ , max(ref_date), by = index_by_friday(ref_date)]
  return(out$V1)
}

last_in_month <- function(dates){
  dates <- data.table("ref_date" = dates)
  out <- dates[ , max(ref_date), by = end_of_month(ref_date)]
  return(out$V1)
}

first_of_month <- function(date) as.Date(paste(year(date), month(date), 01, sep = "-"))

last_in_quarter <- function(dates){
  dates <- data.table("ref_date" = dates)
  out <- dates[ , max(ref_date), by = end_of_quarter(ref_date)]
  return(out$V1)
}

last_in_year <- function(dates){
  dates <- data.table("ref_date" = dates)
  out <- dates[ , max(ref_date), by = year(ref_date)]
  return(out$V1)
}

lag_mixed_freq <- function(x, dates, lags = 3){
  frq <- get_data_frq(x, dates)
  dt <- data.table(dates, x)
  names(dt) <- c("ref_date", "value")
  if(frq%in%c('year', 'quarter', 'month', 'week')){
    dt <- agg_to_freq(dt, id_name = NULL, frq = frq)
    dt <- add_forecast_dates(dt, frq = frq)
  }else{
    add_forecast_dates(dt, frq = "day")
  }
  X <- as.matrix(dt$value)
  X <- stack_obs(X, lags)
  X <- X[-NROW(X), ]
  X <- data.table("ref_date" = dt$ref_date[-seq(lags)], X)
  names(X) <- c("ref_date", paste("lag", seq(lags)))
  return(X)
}

diff_na <- function(x){
  ind <- is.finite(x)
  x[ind] <- c(NA,diff(x[ind]))
  return(x)
}

agg_to_freq_wide <- function(dt, date_name = "ref_date", id_name = "series_name",
                             value_name = "value", frq = "month", dt_is_wide = FALSE){
  if(!frq%in%c("week", "month", "quarter", "year")) stop("'frq' must be one of 'week', 'month', 'quarter', or 'year'")
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

format_weekly <- function(dt, date_name = "ref_date"){
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

allNA <- function(x) all(is.na(x))

is_in <- function(that, this_in) this_in%in%that

rollmean <- function(x, n){
  y <- rollmean_cpp(x,n)
  y[!is.finite(y)] <- NA
  return(y)
}




