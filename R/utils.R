#library(data.table)

pct_chng <- function(y){
  y <- as.matrix(y)
  y <- (y[-1, ,drop = FALSE] - y[-NROW(y), ,drop = FALSE])/y[-NROW(y), ,drop = FALSE]
  y <- rbind(matrix(NA, 1, NCOL(y)), y) # keep the number of rows the same
  return(y)
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
    setnames(dt_out, "V1", "value")
    setnames(dt_out, "index_by_friday", "ref_date")
    dt_out <- merge(data.table("ref_date" = seq.Date(from = min(dt_out$ref_date), to = max(dt_out$ref_date), by = 7)),
                    dt_out, all = TRUE) # regularize frequency
  }else if(frq == "month"){
    dt_out <- dt_long[ , mean_na(value),
                       by = .(series_name, end_of_month(ref_date))]
    setnames(dt_out, "V1", "value")
    setnames(dt_out, "end_of_month", "ref_date")
    tmp_dt <- data.table("ref_date" = end_of_month(seq.Date(from = first_of_month(min(dt_out$ref_date)),
                                               to = first_of_month(max(dt_out$ref_date)), by = "month")))
    dt_out <- merge(tmp_dt, dt_out, all = TRUE) # regularize frequency
  }else if(frq == "quarter"){
    dt_out <- dt_long[ , mean_na(value),
                       by = .(series_name, end_of_quarter(ref_date))]
    setnames(dt_out, "V1", "value")
    setnames(dt_out, "end_of_quarter", "ref_date")
    tmp_dt <- data.table("ref_date" = end_of_quarter(seq.Date(from = first_of_month(min(dt_out$ref_date)),
                                                            to = first_of_month(max(dt_out$ref_date)), by = "quarter")))
    dt_out <- merge(tmp_dt, dt_out, all = TRUE) # regularize frequency
  }else if(frq == "year"){
    dt_out <- dt_long[ , mean_na(value),
                       by = .(series_name, end_of_year(ref_date))]
    setnames(dt_out, "V1", "value")
    setnames(dt_out, "end_of_year", "ref_date")
    tmp_dt <- data.table("ref_date" = seq.Date(from = min(dt_out$ref_date),
                                               to = max(dt_out$ref_date), by = "year"))
    dt_out <- merge(tmp_dt, dt_out, all = TRUE) # regularize frequency
  }else{
    stop("'frq' must be one of 'week', 'month', 'quarter', or 'year'")
  }
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

count_obs <- function(x){
  if(all(!is.finite(x))){
    return(as.integer(NA))
  }else{
    return(as.integer(sum(is.finite(x))))
  }
}

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

agg_to_freq_wide <- function(dt, date_name = "ref_date", frq = "month"){
  setnames(dt, date_name, "ref_date")
  dt_long <- melt(dt, id.vars = 'ref_date', variable.name = "series_name", na.rm = TRUE)
  dt_long <- agg_to_freq(dt_long, frq = "week")
  dt <- dcast(dt_long, ref_date ~ series_name, value.var = "value", fun.aggregate = mean_na)
  return(dt)
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

auto_pub_lag <- function(ref, pub){
  if(all(is.na(pub))){
    return(round(median(diff(ref), na.rm = TRUE)/2))
  }else{
    return(round(median(pub - ref, na.rm = TRUE)))
  }
}



fill_pub_dates <- function(input_data){
  #create pub_dates if missing
  if("pub_date"%in%colnames(input_data)){
    input_data[ , pub_date := as.Date(pub_date)]
    input_data[pub_date == as.Date("2019-07-05"), pub_date := NA]
    input_data[ , pub_lag := auto_pub_lag(ref_date, pub_date), by = .(country, series_name)]
    input_data[is.na(pub_date), pub_date := ref_date + pub_lag, by = .(country, series_name)]
  }else{
    input_data[ , pub_lag := round(median(diff(ref_date), na.rm = TRUE)/2), by = .(country, series_name)]
    input_data[ , pub_date := ref_date + pub_lag, by = .(country, series_name)]
  }
  return(input_data)
}

