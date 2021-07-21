

pct_by_month <- function(response, dt){
  out <- dt[ , sum(value == response)/.N, by = end_of_month(ref_date)]
  names(out) <- c("ref_date", response)
  return(out)
}  

pct_by_week <- function(response, dt){
  out <- dt[ , sum(value == response)/.N, by = index_by_friday(ref_date)]
  names(out) <- c("ref_date", response)
  return(out)
}  

pct_by_quarter <- function(response, dt){
  out <- dt[ , sum(value == response)/.N, by = end_of_quarter(ref_date)]
  names(out) <- c("ref_date", response)
  return(out)
}  


tot_by_month <- function(response, dt){
  out <- dt[ , sum(value == response), by = end_of_month(ref_date)]
  names(out) <- c("ref_date", response)
  return(out)
}  

tot_by_week <- function(response, dt){
  out <- dt[ , sum(value == response), by = index_by_friday(ref_date)]
  names(out) <- c("ref_date", response)
  return(out)
}  

tot_by_quarter <- function(response, dt){
  out <- dt[ , sum(value == response), by = end_of_quarter(ref_date)]
  names(out) <- c("ref_date", response)
  return(out)
}  

pct_total <- function(dt, col_name){
  x <- as.matrix(dt[ , col_name, with = FALSE])
  x <- x[x!="" & !is.na(x)]
  resp <- unique(x)
  out <- sapply(resp, FUN = function(j) sum(x==j))
  out <- out/length(x)
  return(out)
}

#' Percent of responses at a given frequency
#' 
#' Return the percent of responses to categorical answers at a specified frequency
#' 
#' @param dt data table of responses
#' @param col_name name of column containing responses
#' @param by frequency of response aggregation, one of `"month"`, `"quarter"`, `"week"`
#' @param date_name name of column containing dates 
pct_response <- function(dt, col_name = NULL, by = c("month", "quarter", "week"), date_name = "ref_date"){
  dt <- data.table(dt)
  setnames(dt, date_name, "ref_date")
  setcolorder(dt, "ref_date")
  if(NCOL(dt)!=2){
    dt <- dt[ , c("ref_date", col_name), with = FALSE]
  }
  names(dt)[2] <- "value"
  dt <- dt[value != ""] # drop empty responses
  dt <- dt[!is.na(value)]
  resp <- unique(dt$value)
  if(tolower(substring(by,1,1))=="m"){
    out <- lapply(resp, pct_by_month, dt=dt)
    out <- Reduce(function(...) merge(..., all = TRUE), out)
  }else if(tolower(substring(by,1,1))=="w"){
    out <- lapply(resp, pct_by_week, dt=dt)
    out <- Reduce(function(...) merge(..., all = TRUE), out)
  }else if(tolower(substring(by,1,1))=="q"){
    out <- lapply(resp, pct_by_quarter, dt=dt)
    out <- Reduce(function(...) merge(..., all = TRUE), out)
  }else{
    stop("by must be month, week, or quarter")
  }
  return(out)
}

#' Number of of responses at a given frequency
#' 
#' Return the total number of responses to categorical answers at a specified frequency
#' 
#' @param dt data table of responses
#' @param col_name name of column containing responses
#' @param by frequency of response aggregation, one of `"month"`, `"quarter"`, `"week"`
#' @param date_name name of column containing dates 
total_response <- function(dt, col_name = NULL, by = "month", date_name = "ref_date"){
  dt <- data.table(dt)
  setnames(dt, date_name, "ref_date")
  setcolorder(dt, "ref_date")
  if(NCOL(dt)!=2){
    dt <- dt[ , c("ref_date", col_name), with = FALSE]
  }
  names(dt)[2] <- "value"
  dt <- dt[value != ""] # drop empty responses
  dt <- dt[!is.na(value)]
  resp <- unique(dt$value)
  if(tolower(substring(by,1,1))=="m"){
    out <- lapply(resp, tot_by_month, dt=dt)
    out <- Reduce(function(...) merge(..., all = TRUE), out)
  }else if(tolower(substring(by,1,1))=="w"){
    out <- lapply(resp, tot_by_week, dt=dt)
    out <- Reduce(function(...) merge(..., all = TRUE), out)
  }else if(tolower(substring(by,1,1))=="q"){
    out <- lapply(resp, tot_by_quarter, dt=dt)
    out <- Reduce(function(...) merge(..., all = TRUE), out)
  }else{
    stop("by must be month, week, or quarter")
  }
  return(out)
}