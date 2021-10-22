# due to data.table syntax, see https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
utils::globalVariables(c(".", "LHS_index", "country", "index_of_observations", "level_value",
                        "low_frequency_trend", "n_obs", "needs_SA", "pub_date", "ref_date",
                        "seasonal_factor", "series_name", "standardize_center", "standardize_scale",
                        "take_diffs", "take_logs", "value"))

try_seas_factor <- function(x, dates, series_name = NULL){
  if(!can_seasonal(dates[is.finite(x)])){
    warning(paste0("Cannot seasonally adjust ", series_name, "; seasonal adjustment requires quarterly or monthly data without internal NA's"))
    return(rep(0, length(x)))
  }
  out <- try_sa(x, dates, transfunc = "none", x11 = TRUE, series_name = series_name)
  return(out$adj_fact)
}

get_lags <- function(y, lags) sapply(lags, FUN = function(j) shift(y, j))

# find the structure of missing observatins in the data and replicate it
rep_missing_obs <- function(lags, LHS, RHS, frq){
  # Get tail of data and identify observations
  last_LHS_obs <- max(LHS[is.finite(value)]$ref_date)
  if(lags>0){
    sq <- LHS[ref_date <= last_LHS_obs]$ref_date
    last_LHS_obs <- sq[length(sq) - lags]
  }
  RHS_tail <- RHS[ref_date>last_LHS_obs]
  RHS_tail[ , index_of_observations := seq(.N), by = series_name]
  RHS_tail[!is.finite(value), index_of_observations := NA]
  
  RHS_0 <- copy(RHS) # make a new copy
  RHS_0[ , LHS_index := end_of_period(ref_date, period = frq, shift = lags)]
  RHS_0[ , index_of_observations := seq(.N), by = .(series_name, LHS_index)]
  
  # Drop observations we do not see in the contemporaneous data
  for(this_series_name in unique(RHS_0$series_name)){
    idx <- RHS_tail[series_name == (this_series_name)]$index_of_observations
    RHS_0 <- RHS_0[!(series_name == (this_series_name)  & !index_of_observations%in%idx)]
  }
  
  # Aggregate to LHS frequency
  RHS_0 <- RHS_0[ , mean_na(value), by = c("series_name", "LHS_index")]
  
  # Convert to wide format
  RHS_0 <- dcast(RHS_0, LHS_index ~ series_name, value.var = 'V1')
  names(RHS_0) <- c("ref_date", paste(names(RHS_0)[-1], lags))
  
  return(RHS_0)
}

cast_LHS <- function(lags, LHS, frq){
  lhs <- copy(LHS)
  lhs[ , ref_date := end_of_period(ref_date, period = frq, shift = lags)]
  lhs <- dcast(lhs, ref_date ~ series_name, value.var = 'value')
  names(lhs) <- c("ref_date", paste(names(lhs)[-1], lags))
  return(lhs)
}

#' Process mixed frequency 
#'
#' Process mixed frequency data for nowcasting applications by identifying the
#' missing observations in the contemporaneous data and replicating this pattern
#' of missing observations in the historical data prior to aggregation. This allows
#' the incorporation of all available information into the model while still using 
#' uniform frequency models to actually generate predictions, and can thus be applied
#' to a wide array of econometrics and machine learning applications.
#' 
#' Right hand side data will always include observations contemporaneous with LHS data. Use `RHS_lags` to add lags
#' of RHS data to the output, and `LHS_lags` to add lags of LHS data to the output. By default the function will return
#' data in long format designed to be used with the `dateutils` function `process()`. Specifying `return_dt = FALSE` will 
#' return LHS variables in the matrix `Y`, RHS variables in the matrix `X`, and corresponding dates (by index) in the
#' date vector `dates`.
#'
#' @param LHS Left hand side data in long format. May include multiple LHS variables, but LHS variance MUST have the same frequency.
#' @param RHS Right hand side data in long format at any frequency.
#' @param LHS_lags Number of lags of LHS variables to include in output.
#' @param RHS_lags Number of lags of RHS variables to include in output (may be 0, indicating contemporaneous values only).
#' @param as_of Backtesting the model "as of" this date; requires that `pub_date` is specified in the data
#' @param frq Frequency of LHS data, one of 'week', 'month', 'quarter', 'year'. If not specified, the function will attempt to automatically identify the frequency.
#' @param date_name Name of date column in data.
#' @param id_name Name of ID column in the data.
#' @param value_name Name of value column in the data.
#' @param pub_date_name Name of publication date in the data.
#' @param return_dt T/F, should the function return a `data.table`? IF FALSE the function will return matrix data.  
#' @return data.table in long format (unless `return_dt = FALSE`). Variables ending in '0' are contemporaneous, ending in '1' are at one lag, '2' at two lags, etc.
#' 
#' @examples  
#' LHS <- fred[series_name == "gdp constant prices"]
#' RHS <- fred[series_name != "gdp constant prices"]
#' dt <- process_MF(LHS, RHS)
process_MF <- function(LHS, RHS, LHS_lags = 1, RHS_lags = 1, as_of = NULL, frq = c('auto', 'week', 'month', 'quarter', 'year'), 
                       date_name = "ref_date", id_name = "series_name", value_name = "value", pub_date_name = "pub_date", 
                       return_dt = TRUE){
  
  frq <- match.arg(frq)
  LHS <- data.table(LHS)
  RHS <- data.table(RHS)
  
  # Sample data to run through the code line by line
  # LHS <- fred[series_name == "gdp constant prices"]
  # RHS <- fred[series_name != "gdp constant prices"]
  # RHS_lags <- 3
  # LHS_lags <- 3
  # as_of <- as.Date("2021-02-15")
  
  # set names of data.tables
  setnames(RHS, date_name, "ref_date")
  setnames(LHS, date_name, "ref_date")
  if(!is.null(id_name)){
    setnames(RHS, id_name, "series_name")
    setnames(LHS, id_name, "series_name")
  }else{
    RHS[ , series_name := "V2"]
    LHS[ , series_name := "V1"]
  }
  setnames(RHS, value_name, "value")
  setnames(LHS, value_name, "value")
  
  if(!is.null(pub_date_name)){
    setnames(LHS, pub_date_name, "pub_date")
    setnames(RHS, pub_date_name, "pub_date")
    if(!is.null(as_of)){ # censor data if given an as_of date
      as_of <- as.Date(as_of)
      LHS <- LHS[pub_date <= as_of]
      RHS <- RHS[pub_date <= as_of]
    }
  }
  
  if('country'%in%names(RHS)){
    if(length(unique(RHS$country))>1){
      RHS[ , series_name := paste(country, series_name)]
    }
  }
  
  if('country'%in%names(LHS)){
    if(length(unique(LHS$country))>1){
      LHS[ , series_name := paste(country, series_name)]
    }
  }
  
  # Set LHS frequency
  if(frq == 'auto'){
    frq <- try(get_data_frq(dates = LHS$ref_date))
    if(inherits(frq, 'try-error')){
      stop('Could not automatically determin frequency of LHS variable')
    }
    if(frq == 'day') stop('LHS data is high frequency')
  }

  RHS_list <- lapply(seq(0, RHS_lags), FUN=rep_missing_obs, LHS=LHS, RHS=RHS, frq=frq)
  rhs <- Reduce(function(...) merge(..., by = "ref_date", all = TRUE), RHS_list) # merge lists together
  
  # LHS lags 
  LHS_list <- lapply(seq(0,LHS_lags), FUN = cast_LHS, LHS = LHS, frq = frq)
  lhs <- Reduce(function(...) merge(..., by = "ref_date", all = TRUE), LHS_list) # merge lists together
  
  out <- merge(lhs, rhs, by = "ref_date", all = TRUE)
  if(return_dt){
    return(melt(out, id.vars = "ref_date", variable.name = "series_name")) # return in long format
  }else{
    k <- length(unique(LHS$series_name))
    Y <- as.matrix(out[ , seq(2,k+1), with = FALSE])
    X <- as.matrix(out[ , seq(k+2,NCOL(out)), with = FALSE])
    dates <- out$ref_date
    return(list(Y = Y,
                X = X,
                dates = dates))
  }
}

#' Process Data
#' 
#' Process data to ensure stationarity in long format for time series modeling
#' 
#' Process data can be used to transform data to insure stationarity and to censor data for backtesting. Directions
#' for processing each file come from the data.table `lib`. This table must include the columns `series_name`, `take_logs`,
#' and `take_diffs`. Unique series may also be identified by a combination of `country` and `series_name`. Optional columns
#' include `needs_SA` for series that need seasonal adjustment, `detrend` for removing low frequency trends (nowcasting only;
#' detrend should not be used for long horizon forecasts), `center` to de-mean the data, and `scale` to scale the data. If the 
#' argument to `process_wide()` of `detrend`, `center`, or `scale` is `FALSE`, the operation will not be performed. If `TRUE`,
#' the function will check for the column of the same name in `lib`. If the column exists, T/F entries from this column are used
#' to determine which series to transform. If the column does not exist, all series will be transformed. 
#' 
#' @param dt Data in long format.
#' @param lib Library with instructions regarding how to process data; see details.
#' @param detrend T/F should data be detrended (see details)?
#' @param center T/F should data be centered (i.e. de-meaned)?
#' @param scale T/F should data be scaled (i.e. variance 1)?
#' @param as_of "As of" date at which to censor observations for backesting. This requires `pub_date_name` is specified.
#' @param date_name Name of data column in the data.
#' @param id_name Name of ID column in the data.
#' @param value_name Name of value column in the data
#' @param pub_date_name Name of publication date column in the data; required if `as_of` specified.
#' @param ignore_numeric_names T/F ignore numeric values in matching series names in `dt` to series names in `lib`. This
#' is required for data aggregated using `process_MF()`, as lags of LHS and RHS data are tagged 0 for contemporaneous data, 
#' 1 for one lag, 2 for 2 lags, etc. Ignoring these tags insures processing from `lib` is correctly identified.
#' @param silent T/F, supress warnings?
#' @return data.table of processed values in long format.

#'@examples
#' dt <- process(fred, fredlib)
#' 
#' LHS <- fred[series_name == "gdp constant prices"]
#' RHS <- fred[series_name != "gdp constant prices"]
#' dtQ <- process_MF(LHS, RHS)
#' dt_processed <- process(dtQ, fredlib)
process <- function(dt, lib, detrend = TRUE, center = TRUE, scale = TRUE, as_of = NULL, date_name = "ref_date", 
                    id_name = "series_name", value_name = "value", pub_date_name = NULL, 
                    ignore_numeric_names = TRUE, silent = FALSE){
  
  lib <- data.table(lib)
  dt <- data.table(dt)
  # set names of data.tables
  setnames(dt, date_name, "ref_date")
  setnames(dt, id_name, "series_name")
  setnames(dt, value_name, "value")
  if(!is.null(pub_date_name)){
    setnames(dt, pub_date_name, "pub_date")
    if(!is.null(as_of)){ # censor data if given an as_of date
      as_of <- as.Date(as_of)
      dt <- dt[pub_date <= as_of]
    }
  }
  
  if(!"take_logs"%in%names(lib)) stop("'take_logs' is not a column in 'lib'")
  if(!"take_diffs"%in%names(lib)) stop("'take_diffs' is not a column in 'lib'")
  if(!"needs_SA"%in%names(lib)){
    lib[ , needs_SA := FALSE]
    if(!silent){
      warning("'needs_SA' is not a column in 'lib', defaulting to 'FALSE' for all series")
    }
  } 
  
  if(!silent){
    if(ignore_numeric_names){
      miss_names <- unique(extract_character(dt$series_name))
      miss_idx <- which(!miss_names%in%extract_character(lib$series_name))
      if(length(miss_idx)>0){
        warning(cat(miss_names[miss_idx], "not found in 'lib', no adjustements will be made"))
      }
    }else{
      miss_names <- unique(dt$series_name)
      miss_idx <- which(!miss_names%in%lib$series_name)
      if(length(miss_idx)>0){
        warning(cat(miss_names[miss_idx], "not found in 'lib', no adjustements will be made"))
      }
    }
  }
  
  if(any(as.logical(lib$take_logs))){
    if("country"%in%names(lib) && "country"%in%names(dt)){
      if(ignore_numeric_names){
        dt[extract_character(series_name)%in%extract_character(lib[as.logical(take_logs)]$series_name) & 
           extract_character(country)%in%extract_character(lib[as.logical(take_logs)]$country), value := log(value)]
      }else{
        dt[series_name%in%lib[as.logical(take_logs)]$series_name & 
           country%in%lib[as.logical(take_logs)]$country, value := log(value)]
      }
    }else{
      if(ignore_numeric_names){
        dt[extract_character(series_name)%in%extract_character(lib[as.logical(take_logs)]$series_name), value := log(value)]
      }else{
        dt[series_name%in%lib[as.logical(take_logs)]$series_name, value := log(value)]
      }
    }
  }
  
  if(any(as.logical(lib$needs_SA))){
    dt[ , seasonal_factor := 0]
    if("country"%in%names(lib) && "country"%in%names(dt)){
      if(ignore_numeric_names){
        dt[extract_character(series_name)%in%extract_character(lib[as.logical(needs_SA)]$series_name) & extract_character(country)%in%extract_character(lib[as.logical(needs_SA)]$country),
           seasonal_factor := try_seas_factor(value, ref_date), by = c("country", "series_name")]
      }else{
        dt[series_name%in%lib[as.logical(needs_SA)]$series_name & country%in%lib[as.logical(needs_SA)]$country,
           seasonal_factor := try_seas_factor(value, ref_date), by = c("country", "series_name")]
      }
    }else{
      if(ignore_numeric_names){
        dt[extract_character(series_name)%in%extract_character(lib[as.logical(needs_SA)]$series_name),
           seasonal_factor := try_seas_factor(value, ref_date), by = "series_name"]
      }else{
        dt[series_name%in%lib[as.logical(needs_SA)]$series_name,
           seasonal_factor := try_seas_factor(value, ref_date), by = "series_name"]
      }
    }
    dt[is.na(seasonal_factor), seasonal_factor := 0]
    dt[ , value := value - seasonal_factor]
  }
  
  dt[ , level_value := value]
  
  if(any(as.logical(lib$take_diffs))){
    if("country"%in%names(lib) && "country"%in%names(dt)){
      if(ignore_numeric_names){
        dt[extract_character(series_name)%in%extract_character(lib[as.logical(take_diffs)]$series_name) & 
           extract_character(country)%in%extract_character(lib[as.logical(take_diffs)]$country), value := Diff(value), by = c("country", "series_name")]
      }else{
        dt[series_name%in%lib[as.logical(take_diffs)]$series_name & 
           country%in%lib[as.logical(take_diffs)]$country, value := Diff(value), by = c("country", "series_name")]
      }
    }else{
      if(ignore_numeric_names){
        dt[extract_character(series_name)%in%extract_character(lib[as.logical(take_diffs)]$series_name), value := Diff(value), by = "series_name"]
      }else{
        dt[series_name%in%lib[as.logical(take_diffs)]$series_name, value := Diff(value), by = "series_name"]
      }
    }
  }
  
  if(detrend){
    if("detrend"%in%names(lib)){ # detrend series in lib
      dt[ , low_frequency_trend := 0]
      if("country"%in%names(lib) && "country"%in%names(dt)){
        if(ignore_numeric_names){
          dt[extract_character(series_name)%in%extract_character(lib[as.logical(detrend)]$series_name) & extract_character(country)%in%extract_character(lib[as.logical(detrend)]$country),
             low_frequency_trend := try_trend(value), by = c("country", "series_name")]
        }else{
          dt[series_name%in%lib[as.logical(detrend)]$series_name & country%in%lib[as.logical(detrend)]$country,
             low_frequency_trend := try_trend(value), by = c("country", "series_name")]
        }
      }else{ # if country not in lib and data
        if(ignore_numeric_names){
          dt[extract_character(series_name)%in%extract_character(lib[as.logical(detrend)]$series_name),
             low_frequency_trend := try_trend(value), by = "series_name"]
        }else{
          dt[series_name%in%lib[as.logical(detrend)]$series_name,
             low_frequency_trend := try_trend(value), by = "series_name"]
        }
      }
    }else{ # detrend all series
      if("country"%in%names(dt)){
        dt[ , low_frequency_trend := try_trend(value), by = c("country", "series_name")]
      }else{
        dt[ , low_frequency_trend := try_trend(value), by = "series_name"]
      }
    }
    dt[is.na(low_frequency_trend), low_frequency_trend := 0]
    dt[ , value := value - low_frequency_trend]
  }
  
  if(center){
    dt[ , standardize_center := 0]
    if("center"%in%names(lib)){
      if("country"%in%names(lib) && "country"%in%names(dt)){
        if(ignore_numeric_names){
          dt[extract_character(series_name)%in%extract_character(lib[as.logical(center)]$series_name) & extract_character(country)%in%extract_character(lib[as.logical(center)]$country),
             standardize_center := mean_na(value), by = c("country", "series_name")]
        }else{
          dt[series_name%in%lib[as.logical(center)]$series_name & country%in%lib[as.logical(center)]$country,
             standardize_center := mean_na(value), by = c("country", "series_name")]
        }
      }else{ # country not a id column
        if(ignore_numeric_names){
          dt[extract_character(series_name)%in%extract_character(lib[as.logical(center)]$series_name),
             standardize_center := mean_na(value), by = "series_name"]
        }else{
          dt[series_name%in%lib[as.logical(center)]$series_name,
             standardize_center := mean_na(value), by = "series_name"]
        }
      }
    }else{ # center all series
      if("country"%in%names(dt)){
        dt[ , standardize_center := mean_na(value), by = c("country", "series_name")]
      }else{
        dt[ , standardize_center := mean_na(value), by = "series_name"]
      }
    }
    dt[is.na(standardize_center), standardize_center := 0]
    dt[ , value := value - standardize_center]
  }
  
  if(scale){
    dt[ , standardize_scale := 1]
    if("scale"%in%names(lib)){
      if("country"%in%names(lib) && "country"%in%names(dt)){
        if(ignore_numeric_names){
          dt[extract_character(series_name)%in%extract_character(lib[as.logical(scale)]$series_name) & extract_character(country)%in%extract_character(lib[as.logical(scale)]$country),
             standardize_scale := sd_na(value), by = c("country", "series_name")]
        }else{
          dt[series_name%in%lib[as.logical(scale)]$series_name & country%in%lib[as.logical(scale)]$country,
             standardize_scale := sd_na(value), by = c("country", "series_name")]
        }
      }else{ # country not an id column
        if(ignore_numeric_names){
          dt[extract_character(series_name)%in%extract_character(lib[as.logical(scale)]$series_name),
             standardize_scale := sd_na(value), by = "series_name"]
        }else{
          dt[series_name%in%lib[as.logical(scale)]$series_name,
             standardize_scale := sd_na(value), by = "series_name"]
        }
      }
    }else{ # scale all series
      if("country"%in%names(dt)){
        dt[ , standardize_scale := sd_na(value), by = c("country", "series_name")]
      }else{
        dt[ , standardize_scale := sd_na(value), by = "series_name"]
      }
    }
    dt[is.na(standardize_scale), standardize_scale := 1]
    dt[ , value := value/standardize_scale]
  }
  return(dt)
}
  

#' Process Wide Format Data
#' 
#' Process data in wide format for time series modeling
#' 
#' `process_wide()` can be used to transform wide data to insure stationarity. Censoring by pub_date requires long format. Directions
#' for processing each file come from the data.table `lib`. This table must include the columns `series_name`, `take_logs`,
#' and `take_diffs`. Unique series may also be identified by a combination of `country` and `series_name`. Optional columns
#' include `needs_SA` for series that need seasonal adjustment, `detrend` for removing low frequency trends (nowcasting only;
#' `detrend` should not be used for long horizon forecasts), `center` to de-mean the data, and `scale` to scale the data. If the 
#' argument to `process_wide()` of `detrend`, `center`, or `scale` is `FALSE`, the operation will not be performed. If `TRUE`,
#' the function will check for the column of the same name in `lib`. If the column exists, T/F entries from this column are used
#' to determine which series to transform. If the column does not exist, all series will be transformed. 
#' 
#' @param dt_wide Data in wide format.
#' @param lib Library with instructions regarding how to process data; see details.
#' @param detrend T/F should data be detrended (see details)?
#' @param center T/F should data be centered (i.e. de-meaned)?
#' @param scale T/F should data be scaled (i.e. variance 1)?
#' @param date_name Name of data column in the data.
#' @param ignore_numeric_names T/F ignore numeric values in matching series names in `dt` to series names in `lib`. This
#' is required for data aggregated using `process_MF()`, as lags of LHS and RHS data are tagged 0 for contemporaneous data, 
#' 1 for one lag, 2 for 2 lags, etc. Ignoring these tags insures processing from `lib` is correctly identified.
#' @param silent T/F, supress warnings?
#' @return data.table of processed data
#' 
#'@examples
#' LHS <- fred[series_name == "gdp constant prices"]
#' RHS <- fred[series_name != "gdp constant prices"]
#' dtQ <- process_MF(LHS, RHS)
#' dt_wide <- data.table::dcast(dtQ, ref_date ~ series_name, value.var = "value")
#' dt_processed <- process_wide(dt_wide, fredlib)
process_wide <- function(dt_wide, lib, detrend = TRUE, center = TRUE, scale = TRUE, date_name = "ref_date", 
                         ignore_numeric_names = TRUE, silent = FALSE){
  
  dt_wide <- data.table(dt_wide)
  setnames(dt_wide, date_name, "ref_date")  
  dt_long <- melt(dt_wide, id.vars = "ref_date", variable.name = "series_name") # convert to long format
  
  out <- process(dt_long, lib, detrend = detrend, center = center, scale = scale, as_of = NULL, pub_date_name = NULL, 
           ignore_numeric_names = ignore_numeric_names, silent = silent)
  
  return(out)
}



