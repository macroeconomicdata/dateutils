extract_numeric <- function(x) as.numeric(gsub("[^0-9.-]+", "", as.character(x)))

extract_character <- function(x) trimws(gsub("([^A-Za-z ]|NA)","",as.character(x)))


limit_character <- function(x) substr(gsub("([^A-Za-z0-9 ]|NA)","",as.character(x)), 1, 100)


seasonally_adjust_TE <- function(DT, sa_vars, series_names = "series_name", 
                                 value_name = "value", date_name = "ref_date"){
  setnames(DT, value_name, "value")
  setnames(DT, series_names, "series_name")
  setnames(DT, date_name, "ref_date")
  DT_SA <- seas_df_long(DT, sa_names = sa_vars)
  return(DT_SA$values_sa)
}

TE_indicator_data <- function(indicator, country){
  tmp <- try(getIndicatorData(country = country, indicator = indicator))
  if(inherits(tmp,"try-error")){
    #OUT <- data.table("series_name" = tgt, "forecast_date" = as_of, "ref_date" = NULL, "median" = NULL)
    cat("Trading Economics request for ", indicator," for country ", country, "faild. Reason: ", attr(tmp,"condition"))
    return(list())
  }else{
    return(tmp)
  }
} 

TE_historical_data <- function(indicator, country, initDate, endDate = NULL){
  cat("\nPulling ", indicator, " data for ", country, "\n")
  tmp <- try(getHistoricalData(country = country, indicator = indicator, initDate = initDate, endDate = endDate))
  if(inherits(tmp,"try-error")){
    #OUT <- data.table("series_name" = tgt, "forecast_date" = as_of, "ref_date" = NULL, "median" = NULL)
    #cat("Trading Economics request for ", indicator," for country ", country, "faild. Reason: ", attr(tmp,"condition"))
    return(list())
  }else{
    return(tmp)
  }
}

TE_calendar_data <- function(indicator, country, initDate, endDate = NULL){
  cat("\nPulling ", indicator, " calendar data for ", country, "\n")
  tmp <- try(getCalendarData(country = country, indicator = indicator, initDate = initDate, endDate = endDate))
  if(inherits(tmp,"try-error")){
    #OUT <- data.table("series_name" = tgt, "forecast_date" = as_of, "ref_date" = NULL, "median" = NULL)
    #cat("Trading Economics request for ", indicator," for country ", country, "faild. Reason: ", attr(tmp,"condition"))
    return(list())
  }else{
    return(tmp)
  }
}

#helper function
Reference_to_date <- function(te_ref, date_of_pub){
  # row <- PD[idx, ]
  ref_date <- as.character(te_ref)
  if(ref_date == ""){
    return(date_of_pub)
  }else{
    dash <- regexpr("-", ref_date)
    if(dash>0){
      ref_date <- substr(ref_date, dash+1, nchar(ref_date))
    }
    if(grepl("jan", ref_date, ignore.case = T)){
      mnth <- 1
      ref_date <- sub("jan", "", ref_date, ignore.case = T)
    }else if(grepl("feb", ref_date, ignore.case = T)){
      mnth <- 2
      sub("feb", "", ref_date, ignore.case = T)
    }else if(grepl("mar", ref_date, ignore.case = T)){
      mnth <- 3
      sub("mar", "", ref_date, ignore.case = T)
    }else if(grepl("apr", ref_date, ignore.case = T)){
      mnth <- 4
      sub("apr", "", ref_date, ignore.case = T)
    }else if(grepl("may", ref_date, ignore.case = T)){
      mnth <- 5
      sub("may", "", ref_date, ignore.case = T)
    }else if(grepl("jun", ref_date, ignore.case = T)){
      mnth <- 6
      sub("jun", "", ref_date, ignore.case = T)
    }else if(grepl("jul", ref_date, ignore.case = T)){
      mnth <- 7
      sub("jul", "", ref_date, ignore.case = T)
    }else if(grepl("aug", ref_date, ignore.case = T)){
      mnth <- 8
      sub("aug", "", ref_date, ignore.case = T)
    }else if(grepl("sep", ref_date, ignore.case = T)){
      mnth <- 9
      sub("sep", "", ref_date, ignore.case = T)
    }else if(grepl("oct", ref_date, ignore.case = T)){
      mnth <- 10
      sub("oct", "", ref_date, ignore.case = T)
    }else if(grepl("nov", ref_date, ignore.case = T)){
      mnth <- 11
      sub("nov", "", ref_date, ignore.case = T)
    }else if(grepl("dec", ref_date, ignore.case = T)){
      mnth <- 12
      sub("dec", "", ref_date, ignore.case = T)
    }else{
      mnth <- 12
    }
    pub_month <- month(date_of_pub)
    year <- year(date_of_pub)
    if(mnth>pub_month){
      year <- year - 1
    }
    day <- as.numeric(gsub("\\D+", "", ref_date))
    if(is.na(day)){
      day <- MonthDays(year, mnth)
    }
    if(day>31 ){ #year is included
      day <- MonthDays(year, mnth)
    }
    return(as.Date(paste(year, mnth, day, sep = "-")))
  }
}

get_quarter_ref <- function(ref, pub_date){
  possible_dates <- seq.Date(from = first_previous_quarter(as.Date(pub_date)), length.out = 4, by = "-1 quarter")
  if(grepl("Q1", ref)){
    return(end_of_quarter(possible_dates[month(possible_dates)==1]))
  }else if(grepl("Q2", ref)){
    return(end_of_quarter(possible_dates[month(possible_dates)==4]))
  }else if(grepl("Q3", ref)){
    return(end_of_quarter(possible_dates[month(possible_dates)==7]))
  }else if(grepl("Q4", ref)){
    return(end_of_quarter(possible_dates[month(possible_dates)==10]))
  }else{
    return(as.Date(NA))
  }
}