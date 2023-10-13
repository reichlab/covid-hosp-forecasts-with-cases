## functions to create SARIMAX model 

## define transformation and inverse of transformation
fourth_rt<- function(x) {x^(1/4)}
truncated_inv_fourth_rt <- function(x) {pmax(0,x)^4 }
fourth_rt_transformation <- fabletools::new_transformation(fourth_rt, truncated_inv_fourth_rt)

#' Build SARIMA(X) forecast for hospitalization
#'
#' @param data tsibble with index target_end_date, variable hosps, and optional cases column
#' @param case_col name of column with case data to use in model
#' @param p ARIMA lag parameter
#' @param P seasonal ARIMA lag parameter
#' @param d ARIMA difference parameter
#' @param D seasonal ARIMA difference parameter
#'
#' @return a list with three components: `forecasts`, `case_model`, and `hosp_model`
#'
arima_hosp_forecasts <- function(data, case_col, p=0, P=0, d=0, D=0) {
  require(tidyverse)
  require(fable)
  
  # TODO: add support for case_col = NULL, set as default
  
  if(!(case_col %in% colnames(data)))
    stop("case_col argument must match a column name in data.")
  
  ## fix specified case column in data
  data$cases <- pull(data, case_col)
  
  model_cases_by_source <- data[c("target_end_date", "cases")] %>% 
    model(ARIMA(fourth_rt_transformation(cases)))
  
  new_case_by_source_data <- model_cases_by_source %>% 
    forecast(h=28) %>%
    as_tsibble() %>%
    filter(target_end_date > forecast_date) %>%
    select(target_end_date, cases = .mean)

  hosps_and_case_by_date <- data
  
  ## function to transform hospitalizations 
  response_var <- "fourth_rt_transformation(hosps)"
   
  ## build RHS of formula and needed variables for values of p
  formula <- paste0(response_var, " ~ ",
                    paste0("hosp_lag", seq_len(p), collapse = " + "), 
                    " + ", 
                    paste0("case_lag", seq_len(p), collapse = " + "))
  ## add seasonal terms if P>0
  seasonal_terms <- ""
  for (seasonal_lag in seq_len(P)) {
    
    ## add terms to formula
    seasonal_terms <- paste0(
      seasonal_terms, " + ",
      paste0("hosp_lag", seq(from=0, to=p) + seasonal_lag * 7, collapse = " + "), " + ",
      paste0("case_lag", seq(from=0, to=p) + seasonal_lag * 7, collapse = " + "))
  }
  formula <- as.formula(paste0(formula, seasonal_terms))
  print(formula) #print formula to view lags 
  
  ## add needed lags to the data
  hosps_and_case_by_date_w_lags <- hosps_and_case_by_date %>% 
    add_lags("cases", p=p, P=P, d=d, D=D) %>%
    add_lags("hosps", p=p, P=P, d=d, D=D) 
  
  ## fit model for hosps
  hosps_and_case_model <- hosps_and_case_by_date %>%
    model(ARIMA(formula))
  
  ## forecast from hosps model
  hosp_forecast_df <- hosps_and_case_model %>% 
    forecast(new_data = new_case_by_source_data, 
             point_forecast = list(.mean = mean, .median = median)) 
  
  return(list(forecasts = hosp_forecast_df, 
              case_model = model_cases_by_source, 
              hosp_model = hosps_and_case_model))
}


#' Adding columns to data for SARIMA models
#'
#' @param data a tsibble that needs new columns
#' @param col_name column name to add lags for
#' @param p ARIMA lag parameter
#' @param P seasonal ARIMA lag parameter
#' @param d ARIMA difference parameter
#' @param D seasonal ARIMA difference parameter
#'
#' @return tsibble with appropriate new columns
#'
add_lags <- function(data, col_name, p, P, d, D) {
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
