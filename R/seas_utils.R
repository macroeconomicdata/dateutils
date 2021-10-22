
match_index_helper <- function(this, that){
  out <- which(that==this)
  if(length(out)==0){
    return(NA)
  }else{
    return(out)
  }
} 

#' Match index values 
#'
#' Match index values of this to that
#' 
#' @param this first object 
#' @param that second object
#' @return A list of indexes indicating the elements that are matched to each other
#' 
#' @examples 
#' match_index(c(1,2,3),c(2,3,4)) ## $that_idx: 1 2; $this_idx: 2 3
match_index <- function(this, that){
  idx <- sapply(this, FUN = match_index_helper, that)
  return(list(that_idx = idx[!is.na(idx)],
              this_idx = which(!is.na(idx))))
}

#' Tabular data to ts() format
#' 
#' transform data in 'x' corresponding to dates in 'dates' to ts() format
#' 
#' @param x data 
#' @param dates dates
#' @return data in ts() format
#' 
#' @examples 
#' x <- c(1,2,3,4)
#' dates <- as.Date(c("2020-1-1","2020-2-1","2020-3-1","2020-4-1"))
#' to_ts(x, dates) 
to_ts <- function(x, dates){
  dates <- as.Date(dates) #just in case we forget!
  x <- as.matrix(x)
  frq <- median(diff(dates)) #measured in days
  if(frq >= 27 && frq <= 35){
    dates <- first_of_month(dates)
    sq <- seq.Date(from = dates[1], to = tail(dates,1), by = "month")
    x_in <- matrix(NA, length(sq), NCOL(x))
    idx <- match_index(dates, sq)
    x_in[idx$that_idx, ] <- x[idx$this_idx, ]
    x <- ts(x_in, start = c(year(dates[1]), month(dates[1])), frequency = 12)
  }else if(frq >= 88 && frq <= 94){
    dates <- first_of_quarter(dates)
    sq <- seq.Date(from = dates[1], to = tail(dates,1), by = "quarter")
    x_in <- matrix(NA, length(sq), NCOL(x))
    idx <- match_index(dates, sq)
    x_in[idx$that_idx, ] <- x[idx$this_idx, ]
    x <- ts(x_in, start = c(year(dates[1]), quarter(dates[1])), frequency = 4)
  }else{
    stop("Data must be either monthly or quarterly")
  }
  return(x)
}

#' Can data be seasonally adjusted?
#' 
#' Return a logical indicating whether data at given dates can be seasonally adjusted using seas()
#' 
#' @param dates dates
#' @return A logical variable indicating whether data can be seasonally adjusted
#' 
#' @examples 
#' can_seasonal(fred$ref_date[1:20]) ## TRUE
can_seasonal <- function(dates){
  dates <- as.Date(dates) #just in case we forget!
  frq <- diff(dates) #measured in days
  if(all(frq >= 27 & frq <= 35) || all(frq >= 89 & frq <= 94)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' ts() data to a dataframe
#' 
#' Transform monthly or quarterly ts() data to a dataframe
#' 
#' @param x ts() format data which is either monthly or quarterly
#' @param end_period T/F, for monthly or quarterly data, should dates be indexed to the end of the period?
#' @return Data in dataframe format
#' 
#' @examples 
#' x <- ts(c(1,2,3,4), start=c(2020,1), end=c(2020,4), frequency=4) 
#' ts_to_df(x)
  ts_to_df <- function(x, end_period = TRUE){
  ts_year   <- floor(time(x) + 1e-5)
  ts_month  <- round(12*(time(x) - ts_year)) + 1
  ts_date   <- as.Date(paste(as.character(ts_year),ts_month,"01", sep = "-"))
  frq <- median(diff(ts_date))
  if(end_period){
    if(frq >= 27 && frq <= 35){
      ts_date   <- end_of_period(ts_date)
    }else if(all(frq >= 89 & frq <= 94)){
      ts_date <- end_of_period(ts_date, period = "quarter")
    }else{
      stop("Frequency is not monthly or quarterly")
    }
  }
  df        <- data.frame("ref_date" = ts_date, unclass(x))
  return(df)
}

find_time <- function(from_time, to_time){
  return(which(round(to_time, 5) == round(from_time, 5)))
}

replace_by_time <- function(old_time, new_ts){
  idx <- find_time(old_time, time(new_ts))
  if(length(idx) == 0) return(NA)
  else return(new_ts[idx])
}

#' Match dates between two timeseries
#' 
#' Find values in `new_ts` that correspond to dates in `old_ts`
#' 
#' @param old_ts timeseries data
#' @param new_ts timeseries data
#' @return Timeseries data in which `new_ts` corresponds to `old_ts`
#' 
#' @examples 
#' old_ts <- ts(c(1,2,3,4), start=c(2020,1), end=c(2020,4), frequency=4) 
#' new_ts <- ts(c(5,6,3,4), start=c(2019,4), end=c(2020,3), frequency=4) 
#' match_ts_dates(old_ts, new_ts) 
match_ts_dates <- function(old_ts, new_ts){
  out <- sapply(time(old_ts), FUN = replace_by_time, new_ts = new_ts)
  out <- ts(out, start = attr(old_ts, "tsp")[1], end = attr(old_ts, "tsp")[2],
            frequency = attr(old_ts, "tsp")[3])
  return(out)
}


#' Seasonally adjust data using seas()
#' 
#' Seasonaly adjust monthly or quarterly data using X-13 SEATS via seas()
#' 
#' @param x data
#' @param dates dates corresponding to data 'x'
#' @param x11 T/F, use x11 as opposed to X-13 SEATS
#' @param transfunc Data transformation, one of `none` for no transformation, `auto` for automatic detection, or `log` for log transformation 
#' @return A list with `adj_fact` containing seasonal factors and `sa_final` containing seasonally adjusted data.
#' 
#' @examples 
#' x <- fred[series_name == "gdp constant prices", value]
#' dates <- fred[series_name == "gdp constant prices", ref_date ]
#' run_sa(x, dates, transfunc = "log")
run_sa <- function(x, dates, x11 = FALSE, transfunc = c("none", "auto", "log")){
  transfunc <- match.arg(transfunc) # checks and picks the first if unspecified
  x_ts <- to_ts(x, dates)
  if(x11){
    sa <- seas(x_ts, x11 = "", transform.function = transfunc, x11.appendfcst = "yes")
    if(!is.null(sa$series$d16)){
      adj_fact <- match_ts_dates(x_ts, sa$series$d16) # sa factors
    }else{
      adj_fact <- rep(0, length(x)) # sa factors
    }
  }else{
    sa <- seas(x_ts, transform.function = transfunc, seats.appendfcst = "yes")
    if(!is.null(sa$series$s16)){
      adj_fact <- match_ts_dates(x_ts, sa$series$s16) # sa factors
    }else{
      adj_fact <- to_ts(rep(0, length(x)), dates) # sa factors
    }
  }
  sa_final <- match_ts_dates(x_ts, final(sa))
  sa_final[!is.finite(x_ts)] <- NA
  return(list(adj_fact = unclass(adj_fact),
              sa_final = unclass(sa_final)))
}

#' Seasonally adjust data using seas()
#' 
#' Seasonaly adjust monthly or quarterly data using X-13 SEATS via seas()
#' 
#' @param x data
#' @param dates dates corresponding to data 'x'
#' @param x11 T/F, use x11 as opposed to X-13 SEATS
#' @param transfunc Data transformation, one of `none` for no transformation, `auto` for automatic detection, or `log` for log transformation 
#' @param series_name Include series name to print out if failure (for lapply() applications)
#' @return A list with `adj_fact` containing seasonal factors and `sa_final` containing seasonally adjusted data. 
#' If seasonal adjsutment failed `adj_fact` will contain zeros and `sa_final` will contain the original data. 
#' 
#' @examples 
#' x <- fred[series_name == "gdp constant prices", value]
#' dates <- fred[series_name == "gdp constant prices", ref_date ]
#' try_sa(x, dates, transfunc = "log")
try_sa <- function(x, dates, x11 = FALSE, transfunc = "none", series_name = NULL){
  out <- try(run_sa(x, dates, x11, transfunc))
  if(inherits(out, "try-error")){
    if(!is.null(series_name)) cat("Seasonal adjustment for", series_name, "failed \n")
    return(list(adj_fact = rep(0, length(x)),
                sa_final = x))
  }else{
    return(out)
  }
}

#' Seasonally adjust wide format data using seas()
#' 
#' Seasonaly adjust multiple monthly or quarterly series in wide format using X-13 SEATS via seas()
#' 
#' @param df wide format dataframe
#' @param sa_cols names or column indexes of series to seasonally adjust
#' @param x11 T/F, use x11 as opposed to X-13 SEATS
#' @param transfunc Data transformation, one of `none` for no transformation, `auto` for automatic detection, or `log` for log transformation
#' @return A list with data.frames `sa_factors` containing seasonal factors and `values_sa` containing seasonally adjusted data.
#' 
#' @examples  
#' seas_df_wide(fred[series_name == "gdp constant prices"], sa_cols="value")
seas_df_wide <- function(df, sa_cols, x11 = FALSE, transfunc = 'none'){
  sa_list <- lapply(df[ , sa_cols, with = FALSE],  FUN = try_sa, dates = df$ref_date, x11 = x11, transfunc = transfunc)
  values_sa <- data.frame(ref_date = df$ref_date,
                          do.call("cbind", lapply(sa_list, FUN = get_from_list, what = "sa_final")))
  sa_factors <- data.frame(ref_date = df$ref_date,
                           do.call("cbind", lapply(sa_list, FUN = get_from_list, what = "adj_fact")))
  if("data.table"%in%class(df)){
    values_sa <- data.table(values_sa)
    sa_factors <- data.table(sa_factors)
  }
  return(list(values_sa = values_sa,
              sa_factors = sa_factors))
}

sa_dt_long <- function(sa_name, dt, x11 = FALSE, transfunc = "none"){
  out <- try_sa(dt[series_name == (sa_name)]$value, dt[series_name == (sa_name)]$ref_date, x11 = x11, transfunc = transfunc, series_name = sa_name)
  values_sa <- data.table(dt[series_name == (sa_name)]$ref_date, paste(sa_name, "sa"), unclass(out$sa_final)) 
  names(values_sa) <- c("ref_date", "series_name", "value")
  sa_factors <- data.table(dt[series_name == (sa_name)]$ref_date, paste(sa_name, "sa factor"), unclass(out$adj_fact)) 
  names(sa_factors) <- c("ref_date", "series_name", "value")
  return(list(values_sa = values_sa,
              sa_factors = sa_factors))
} 

#' Seasonally adjust long format data using seas()
#' 
#' Seasonaly adjust multiple monthly or quarterly series in long format using X-13 SEATS via seas()
#' 
#' @param df long format dataframe
#' @param sa_names names of series to seasonally adjust
#' @param x11 T/F, use x11 as opposed to X-13 SEATS
#' @param transfunc Data transformation, one of `none` for no transformation, `auto` for automatic detection, or `log` for log transformation 
#' @param series_names name of column containing series names
#' @param value_var name of column containing values
#' @param date_var name of column containing dates
#' @return A list with data.frames `sa_factors` containing seasonal factors and `values_sa` containing seasonally adjusted data.
#' 
#' @examples 
#' seas_df_long(fred[series_name == "gdp constant prices"], sa_names="value")
seas_df_long <- function(df, sa_names, x11 = FALSE, transfunc = "none", series_names = "series_name", value_var = "value", date_var = "ref_date"){
  df <- data.table(df)
  setnames(df, series_names, "series_name")
  setnames(df, value_var, "value")
  setnames(df, date_var, "ref_date")
  sa_list <- lapply(sa_names,  FUN = sa_dt_long, dt = df, x11 = x11, transfunc = transfunc)
  values_sa <- rbindlist(lapply(sa_list, FUN = get_from_list, what = "values_sa"))
  sa_factors <- rbindlist(lapply(sa_list, FUN = get_from_list, what = "sa_factors"))
  return(list(values_sa = values_sa,
              sa_factors = sa_factors))
}

fill_trend_tail <- function(x){
  idx <- max(which(is.finite(x)))
  n <- length(x) - idx
  if(n==0) return(x)
  d1 <- x[idx] - x[idx-1]
  d2 <- d1 - (x[idx-1] - x[idx-2])
  D2 <- -sign(d1)*(cumsum(rep(abs(d2),n)) + rep(abs(d1)*10/length(x),n))
  D1 <- rep(d1,n) + D2
  D1[sign(D1)!=sign(d1)] <- 0
  x[seq(idx+1,length(x))] <- x[idx] + cumsum(D1)
  return(x)
}

#' Spline fill missing observations
#' 
#' Spline fill missing observations, designed for filling low frequency trend estimates
#' 
#' @param x data with missing observations
#' @return data with interpolated missing observations
#' 
#' @examples
#' spline_fill_trend(c(1,2,3,NA,5)) ## 1 2 3 4 5
spline_fill_trend <- function(x){
  obs <- which(is.finite(x))
  fst <- obs[1]
  lst <- tail(obs,1)
  x[seq(fst,lst)] <- spline(obs,x[obs],n = lst-fst+1)$y
  if(fst>1) x <- rev(fill_trend_tail(rev(x)))
  if(lst<length(x)) x <- fill_trend_tail(x)
  return(x)
}

#' Spline fill missing observations
#' 
#' Spline fill missing observations from the first observation to the last, leaving NA observations in the head and tail
#' 
#' @param x data with missing observations
#' @return data with interpolated missing observations, except at head and tail, which remain NA
#' 
#' @examples 
#' spline_fill_trend(c(NA,1,2,3,NA,5)) ## NA 1 2 3 4 5
spline_fill <- function(x){
  is_finite <- which(is.finite(x))
  first_obs <- min(is_finite)
  last_obs <- max(is_finite)
  tmp <- spline(is_finite, x[is_finite], n = last_obs - first_obs + 1)
  x[seq(first_obs, last_obs)] <- tmp$y
  return(x)
}

#' Estimate low frequnecy trends
#' 
#' Estimate low frequency trends via loess regression. If the function errors, return zeros (i.e. no trend)
#' 
#' @param x data 
#' @param outlier_rm T/F, remove outliers to estimate trends?
#' @param span span for the loess regression
#' @return Estimated trend in the data
#' 
#' @examples 
#' try_trend(c(1,3,6,7,9,11,14,15,17,18))
try_trend <- function(x, outlier_rm = TRUE, span = 0.6){
  trend <- try(loess(x ~ seq(length(x)), na.action = na.exclude, span = span))
  if(inherits(trend, "try-error")) return(rep(0,length(x)))
  if(outlier_rm){
    for(j in seq(3)){
      trend <- predict(trend)
      vnce <- mean((x-trend)^2, na.rm = T)
      x[abs(x-trend) > 3*sqrt(vnce)] <- NA
      trend <- try(loess(x ~ seq(length(x)), na.action = na.exclude, span = span))
      if(inherits(trend, "try-error")) return(rep(0,length(x)))
    }
  }
  return(spline_fill_trend(predict(trend)))
}

#' Remove low frequency trends from data
#' 
#' Estimate low frequency trends via loess regression and remove them. If the function errors, return x (i.e. no trend)
#' 
#' @param x data 
#' @param outlier_rm T/F, remove outliers to estimate trends?
#' @param span span for the loess regression
#' @return Data with trends removed
#' 
#' @examples
#' try_detrend(c(1,3,6,7,9,11,14,15,17,18))
try_detrend <- function(x, outlier_rm = TRUE, span = 0.6){
  x_in <- x
  trend <- try(loess(x ~ seq(length(x)), na.action = na.exclude, span = span))
  if(inherits(trend, "try-error")) return(x_in)
  if(outlier_rm){
    for(j in seq(3)){
      trend <- predict(trend)
      vnce <- mean((x-trend)^2, na.rm = T)
      x[abs(x-trend) > 3*sqrt(vnce)] <- NA
      trend <- try(loess(x ~ seq(length(x)), na.action = na.exclude, span = span))
      if(inherits(trend, "try-error")) return(x_in)
    }
  }
  return(x_in - spline_fill_trend(predict(trend)))
}


