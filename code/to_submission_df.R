#' Reformat hospitalization forecast to submission file
#' 
#' Note: target is "xx day ahead inc hospitalization" and location is "25". 
#' forecast values are rounded down to integers. 
#' 
#' @param forecast data.frame from `forecast(..., point_forecast = list(.mean = mean, .median = median))`
#' @param forecast_date string of date
#' 
#' @return data.frame with columns target_end_date, value, type, quantile
#' forecast_date, target and location
to_submission_df <- function(forecast, forecast_date){
  # prediction interval levels
  interval_level <- c(seq(10, 90, 10), 95, 98)
  
  # create quantile forecast
  forecast<- forecast %>%
    hilo(level = interval_level) %>%
    unpack_hilo(!! paste0(interval_level,"%")) %>%
    dplyr::select(-hosp_val, -.model)
  
  # reformat quantile forecast
  suppressWarnings(quantile_forecast <- forecast %>%
                     select(-.mean) %>%
                     tidyr::pivot_longer(
                       cols = contains(c("%",".median")),
                       names_to = "quantile",
                       values_to = "value"
                     ) %>%
                     # calculate quantile levels from prediction interval levels
                     tidyr::separate(quantile, into = c("PI_level","bounds"), sep = "%_") %>%
                     dplyr::mutate(PI_level = ifelse(PI_level == ".median", 0.5, as.numeric(PI_level)/100),
                                   type = "quantile") %>%
                     dplyr::mutate(quantile = case_when(
                       bounds == "lower" ~ format((1-PI_level)/2, digits=3, nsmall=3),
                       bounds == "upper" ~ format(1-(1-PI_level)/2, digits=3, nsmall=3),
                       is.na(bounds) ~ format(0.5, digits=3, nsmall=3))) %>%
                     dplyr::select(-bounds, -PI_level))
  
  # reformat point forecast
  point_forecast <- forecast %>%
    select(.mean, target_end_date) %>%
    dplyr::mutate(type = "point") %>%
    dplyr::rename(value = .mean)
  
  # bind two data frames and add additional columns
  submission_df <- merge(quantile_forecast, point_forecast,all = TRUE) %>%
    dplyr::mutate(forecast_date = forecast_date,
                  target = paste(as.numeric(difftime(as.Date(target_end_date),as.Date(forecast_date))), "day ahead inc hosp"),
                  location = "25",
                  value = floor(value))
  
  
  return(submission_df)
}
