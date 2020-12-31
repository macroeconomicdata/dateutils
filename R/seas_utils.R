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

run_sa <- function(x, dates){
  x_ts <- to_ts(x, dates)
  sa <- seas(x_ts, transform.function = "none", seats.appendfcst = "yes")
  adj_fact <- match_ts_dates(x_ts, sa$series$s16) # sa factors
  sa_final <- match_ts_dates(x_ts, final(sa))
  sa_final[!is.finite(x_ts)] <- NA
  return(list(adj_fact = adj_fact,
              sa_final = sa_final))
}

try_sa <- function(x, dates){
  out <- try(run_sa(x, dates))
  if(inherits(out, "try-error")){
    return(list(adj_fact = rep(0, length(x)),
                sa_final = x))
  }else{
    return(out)
  }
}

get_from_list <- function(out, what){
  return(out[[what]])
}

seas_df <- function(df, sa_cols){
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
