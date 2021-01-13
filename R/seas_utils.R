# library(seasonal)
# library(data.table)
# library(dateutils)

#' Tabular data to ts() format
to_ts <- function(x, dates){
  dates <- as.Date(dates) #just in case we forget!
  frq <- diff(dates) #measured in days
  if(all(frq >= 27 & frq <= 35)){
    x <- ts(x, start = c(year(dates[1]), month(dates[1])), frequency = 12)
  }else if(all(frq >= 89 & frq <= 94)){
    x <- ts(x, start = c(year(dates[1]), quarter(dates[1])), frequency = 4)
  }else{
    stop("Data must be either monthly or quarterly, and cannot have internal missing values")
  }
  return(x)
}

can_seasonal <- function(dates){
  dates <- as.Date(dates) #just in case we forget!
  frq <- diff(dates) #measured in days
  if(all(frq >= 27 & frq <= 35) || all(frq >= 89 & frq <= 94)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' ts() data to tibble
ts_to_df <- function(x){
  ts_year   <- floor(time(x))
  ts_month  <- round(12*(time(x) - ts_year)) + 1
  ts_date   <- as.Date(paste(as.character(ts_year),ts_month,"01", sep = "-"))
  ts_date   <- end_of_month(ts_date)
  df        <- data.frame("ref_date" = ts_date, "value" = x)
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

match_ts_dates <- function(old_ts, new_ts){
  out <- sapply(time(old_ts), FUN = replace_by_time, new_ts = new_ts)
  out <- ts(out, start = attr(old_ts, "tsp")[1], end = attr(old_ts, "tsp")[2],
            frequency = attr(old_ts, "tsp")[3])
  return(out)
}

run_sa <- function(x, dates, x11 = FALSE){
  x_ts <- to_ts(x, dates)
  if(x11){
    sa <- seas(x_ts, x11 = "", transform.function = "none", x11.appendfcst = "yes")
    if(!is.null(sa$series$d16)){
      adj_fact <- match_ts_dates(x_ts, sa$series$d16) # sa factors
    }else{
      adj_fact <- rep(0, length(x)) # sa factors
    }
  }else{
    sa <- seas(x_ts, transform.function = "none", seats.appendfcst = "yes")
    if(!is.null(sa$series$s16)){
      adj_fact <- match_ts_dates(x_ts, sa$series$s16) # sa factors
    }else{
      adj_fact <- rep(0, length(x)) # sa factors
    }
  }
  sa_final <- match_ts_dates(x_ts, final(sa))
  sa_final[!is.finite(x_ts)] <- NA
  return(list(adj_fact = adj_fact,
              sa_final = sa_final))
}

try_sa <- function(x, dates, x11 = FALSE, series_name = NULL){
  out <- try(run_sa(x, dates, x11))
  if(inherits(out, "try-error")){
    if(!is.null(series_name)) cat("Seasonal adjustment for", series_name, "failed \n")
    return(list(adj_fact = rep(0, length(x)),
                sa_final = x))
  }else{
    return(out)
  }
}

get_from_list <- function(out, what){
  return(out[[what]])
}

seas_df_wide <- function(df, sa_cols){
  sa_list <- lapply(df[ , sa_cols, with = FALSE],  FUN = try_sa, dates = df$ref_date)
  values_sa <- data.frame(ref_date = df$ref_date,
                          do.call("cbind", lapply(sa_list, FUN = get_from_list, what = "sa_final")))
  sa_factors <- data.frame(ref_date = df$ref_date,
                           do.call("cbind", lapply(sa_list, FUN = get_from_list, what = "adj_fact")))
  if("tbl"%in%class(df)){
    values_sa <- tibble(values_sa)
    sa_factors <- tibble(sa_factors)
  }else if("data.table"%in%class(df)){
    values_sa <- data.table(values_sa)
    sa_factors <- data.table(sa_factors)
  }
  return(list(values_sa = values_sa,
              sa_factors = sa_factors))
}

sa_dt_long <- function(sa_name, dt){
  out <- try_sa(dt[series_name == (sa_name)]$value, dt[series_name == (sa_name)]$ref_date, series_name = sa_name)
  values_sa <- data.table(dt[series_name == (sa_name)]$ref_date, paste(sa_name, "sa"), unclass(out$sa_final)) 
  names(values_sa) <- c("ref_date", "series_name", "value")
  sa_factors <- data.table(dt[series_name == (sa_name)]$ref_date, paste(sa_name, "sa factor"), unclass(out$adj_fact)) 
  names(sa_factors) <- c("ref_date", "series_name", "value")
  return(list(values_sa = values_sa,
              sa_factors = sa_factors))
} 

seas_df_long <- function(df, sa_names, series_names = "series_name", value_var = "value", date_var = "ref_date"){
  df <- data.table(df)
  setnames(df, series_names, "series_name")
  setnames(df, value_var, "value")
  setnames(df, date_var, "ref_date")
  sa_list <- lapply(sa_names,  FUN = sa_dt_long, dt = df)
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
  D2 <- -sign(d1)*(cumsum(rep(abs(d2),n)) + rep(1/n,n))
  D1 <- rep(d1,n) + D2
  D1[sign(D1)!=sign(d1)] <- 0
  x[seq(idx+1,length(x))] <- x[idx] + cumsum(D1)
  return(x)
}

spline_fill_trend <- function(x){
  obs <- which(is.finite(x))
  fst <- obs[1]
  lst <- tail(obs,1)
  x[seq(fst,lst)] <- spline(obs,x[obs],n = lst-fst+1)$y
  if(fst>1) x <- rev(fill_trend_tail(rev(x)))
  if(lst<length(x)) x <- fill_trend_tail(x)
  return(x)
}

try_trend <- function(x, outlier_rm = TRUE){
  trend <- try(loess(x ~ seq(length(x)), na.action = na.exclude, span = .6))
  if(inherits(trend, "try-error")) return(rep(0,length(x)))
  trend <- predict(trend)
  vnce <- mean((x-trend)^2, na.rm = T)
  x[abs(x-trend) > 3*sqrt(vnce)] <- NA
  trend <- try(loess(x ~ seq(length(x)), na.action = na.exclude, span = .6))
  if(inherits(trend, "try-error")){
    return(rep(0,length(x)))
  }else{
    return(spline_fill_trend(predict(trend)))
  } 
}

try_detrend <- function(x, outlier_rm = TRUE){
  x_in <- x
  trend <- try(loess(x ~ seq(length(x)), na.action = na.exclude, span = .6))
  if(inherits(trend, "try-error")) return(x)
  trend <- predict(trend)
  vnce <- mean((x-trend)^2, na.rm = T)
  x[abs(x-trend) > 3*sqrt(vnce)] <- NA
  trend <- try(loess(x ~ seq(length(x)), na.action = na.exclude, span = .6))
  if(inherits(trend, "try-error")){
    return(x)
  }else{
    return(x_in - spline_fill_trend(predict(trend)))
  }
}


