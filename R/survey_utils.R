
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

pct_response <- function(dt, col_name = NULL, by = "month", date_name = "ref_date"){
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
    error("by must be month, week, or quarter")
  }
  return(out)
}


total_response <- function(dt, col_name = NULL, by = "month", date_name = "ref_date"){
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
    error("by must be month, week, or quarter")
  }
  return(out)
}