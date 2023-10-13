## functions to create SARIMAX model 

## define transformation and inverse of transformation
fourth_rt<- function(x) {x^(1/4)}
truncated_inv_fourth_rt <- function(x) {pmax(0,x)^4 }
fourth_rt_transformation <- fabletools::new_transformation(fourth_rt, truncated_inv_fourth_rt)

#' Build SARIMA(X) forecast for hospitalization
#'
#' @param data tsibble with index target_end_date, variable hosps, and optional cases column
#' @param case_col name of column with case data to use in model
#' @param case_p integer representing the number of lags to include for cases
#' @param p ARIMA lag parameter
#' @param P seasonal ARIMA lag parameter
#' @param d ARIMA difference parameter
#' @param D seasonal ARIMA difference parameter
#'
#' @return a list with three components: `forecasts`, `case_model`, and `hosp_model`
#'
arima_hosp_forecasts <- function(data, case_col, case_p, p=0, P=0, d=0, D=0) {
  require(tidyverse)
  require(fable)
  
  # TODO: add support for case_col = NULL, set as default
  # TODO: consider adding arguments to have different lags for case predictors
  
  if(!(case_col %in% colnames(data))) {
    stop("case_col argument must match a column name in data.")
  } else{
    case_P <- 0
  }
  
  forecast_date <- max(data$target_end_date)
  
  ## fix specified case column in data
  data$cases <- pull(data, case_col)
  
  cases_model <- data[c("target_end_date", "cases")] %>% 
    model(ARIMA(fourth_rt_transformation(cases)))
  
  ## forecasted case data with needed lags
  new_data_cases <- cases_model %>% 
    forecast(h=28) %>%
    as_tsibble() %>%
    select(target_end_date, cases = .mean) %>%
    bind_rows(data) %>%
    arrange(target_end_date) %>%
    mutate(cases = fourth_rt(cases)) %>%
    add_lags(col_name = "cases", p=case_p, P=case_P) %>%
    filter(target_end_date > forecast_date)
    
  ## fit model for hosps
  if(is.null(case_col)) {
    ## for model with no predictors, forcing LM with dummy indicator column
    hosps_model <- data %>%
      mutate(x1 = 1) %>%
      model(ARIMA(fourth_rt_transformation(hosps) ~ x1 - 1 + pdq(p=p, d=d, q=0) + PDQ(P=P, D=D, Q=0)))
  } else {
    ## for model with predictors
    
    ## build formula for hosp model
    response_var <- "fourth_rt_transformation(hosps)"
    ## add simple lags
    fmla <- paste0(response_var, " ~ 1 + pdq(p=p, d=d, q=0) + PDQ(P=P, D=D, Q=0)")
    if(case_p > 0)
      fmla <- paste(fmla, paste0("+ cases_lag", seq_len(case_p), collapse = " "))
    ## add seasonal terms if P>0
    seasonal_terms <- ""
    for (seasonal_lag in seq_len(case_P)) {
      ## add terms to formula
      seasonal_terms <- paste0(
        seasonal_terms, " + ",
        paste0("cases_lag", seq(from=0, to=case_p) + seasonal_lag * 7, collapse = " + "))
    }
    fmla <- formula(paste(fmla, seasonal_terms, collapse = " "))
    hosps_model <- data %>%
      add_lags(col_name = "cases", p=case_p, P=case_P) %>%
      model(ARIMA(fmla))
  } 
  
  ## forecast from hosps model
  hosp_forecast_df <- hosps_model %>% 
    forecast(new_data = new_data_cases, 
             point_forecast = list(.mean = mean, .median = median)) 
  
  return(list(data = data,
              forecasts = hosp_forecast_df, 
              cases_model = cases_model, 
              hosps_model = hosps_model,
              new_data_cases = new_data_cases))
}


#' Adding columns to data for SARIMA models
#'
#' @param data a tsibble that needs new columns
#' @param col_name column name to add lags for
#' @param p ARIMA lag parameter
#' @param P seasonal ARIMA lag parameter
#'
#' @return tsibble with appropriate new columns
#'
add_lags <- function(data, col_name, p, P) {
  require(tidyverse)
  
  ## add lag columns to data
  output_data <- data
  for(lag_i in seq_len(p)){
    output_data <- output_data %>%
      mutate("{col_name}_lag{lag_i}" := lag(get(col_name), lag_i))
  }
  
  ## add seasonal terms if P>0
  seasonal_terms <- ""
  for (seasonal_lag in seq_len(P)) {
    # add lag columns to data
    for(lag_p in seq(from=0, to=p)){
      this_lag <- lag_p + seasonal_lag * 7
      output_data <- output_data %>%
        mutate("{col_name}_lag{this_lag}" := lag(get(col_name), this_lag))
    }
  }
  return(output_data)
}
