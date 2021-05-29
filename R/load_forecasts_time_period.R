#' Load covid forecasts from local hub repo or Zoltar.  
#' Return the most recent forecast from each model within 
#' a specified time period.
#' 
#' @param models Character vector of model abbreviations.
#' If missing, forecasts for all models that submitted forecasts 
#' meeting the other criteria are returned.
#' @param start_forecast_date_range Start date for specified time period
#' in 'yyyy-mm-dd' format. 
#' @param end_forecast_date_range End date for specified time period
#' in 'yyyy-mm-dd' format.
#' @param last_forecast_date The forecast date of forecasts to retrieve 
#' in 'yyyy-mm-dd' format.
#' @param locations list of fips. Defaults to all locations with available forecasts.
#' @param types Character vector specifying type of forecasts to load: “quantile” 
#' or “point”. Defaults to all types  with available forecasts
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death'). Defaults to all targets.
#' @param source string specifying where forecasts will be loaded from: either 
#' "local_hub_repo" or "zoltar"
#' @param hub_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository
#' @param data_processed_subpath folder within the hub_repo_path that contains
#' forecast submission files.  Defaults to "data-processed/", which is
#' appropriate for the covid19-forecast-hub repository.
#' @param as_of character for date time to load forecasts submitted as of this time from Zoltar.
#' Ignored if \code{source} is \code{"local_hub_repo"}. 
#' It could use the format of one of the three examples: 
#' "2021-01-01", "2020-01-01 01:01:01" and "2020-01-01 01:01:01 UTC".
#' If you would like to set a timezone, it has to be UTC now. 
#' If not, helper function will append the default timezone to your input based on hub parameter. 
#' Default to NULL to load the latest version.
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are "US" and "ECDC"
#' @param verbose a boolean for printing messages on zoltar job status. Default to TRUE.
#'
#' @return data frame with columns model, forecast_date, location, horizon, 
#' temporal_resolution, target_variable, target_end_date, type, quantile, value,
#' location_name, population, geo_type, geo_value, abbreviation
#' 
#' @export
#'
#'
load_forecasts_time_period <- function(
  models = NULL,
  start_forecast_date_range,
  end_forecast_date_range, 
  locations = NULL,
  types = NULL,
  targets = NULL,
  source = "local_hub_repo",
  hub_repo_path,
  as_of = NULL,
  hub = c("US", "ECDC"),
  verbose = TRUE) {
  
  # validate source
  source <- match.arg(source, choices = c("local_hub_repo", "zoltar"))
  
  # check date format and generate dates of forecasts to load
  forecast_dates <- tryCatch({
    as.character(as.Date(start_forecast_date_range)) 
    as.character(as.Date(end_forecast_date_range))
    as.character(seq(as.Date(start_forecast_date_range), as.Date(end_forecast_date_range), by = "7 days"))
  }, error = function(err){
    stop("Error in load_latest_forecasts: Please provide a valid date object or
           string in format YYYY-MM-DD in start_forecast_date_range or end_forecast_date_range")
  }
  )
  
  if (source == "local_hub_repo") {
    # validate hub repo path
    if (missing(hub_repo_path) | !dir.exists(hub_repo_path)) {
      stop("Error in load_latest_forecasts: Please provide a vaid path to hub repo.")
    } 
    
    # path to data-processed folder in hub repo
    data_processed <- file.path(hub_repo_path, "data-processed/")
    
    forecasts <- load_latest_forecasts_repo(file_path = data_processed, 
                                            models = models, 
                                            forecast_dates = forecast_dates, 
                                            locations = locations, 
                                            types = types, 
                                            targets = targets, 
                                            hub = hub, 
                                            verbose = verbose)
  } else {
    forecasts <- load_latest_forecasts_zoltar(models = models, 
                                              forecast_dates = forecast_dates,
                                              locations = locations, 
                                              types = types,
                                              targets = targets,
                                              as_of = as_of,
                                              hub = hub,
                                              verbose = verbose)
  }
  
  return(forecasts)
}