## draft script for running forecasts in validation phase
## March 2022

## load packages, scripts
library(tidyverse)
library(lubridate)
library(fable)
library(covidHubUtils)
library(covidData)


source("code/hosp_arima_forecast_function.R")


# TODO: 
# - download Monday versions of data from Oct 2020 - Dec 2020 and add to main test data csv file, change first_forecast_date
# - fill in missing datapoints?
# - add back in d=0:1, D=0:1

### define global parameters

## choosing this so that with a 1 week differencing the modeling might start around Aug 1
## also, there were odd data anomalies in hospitalizations on July 21/22
data_start_date <- ymd("2020-07-24")

## roughly start of winter wave in MA, also give us a few weeks of data to fit the model on early
first_forecast_date <- ymd("2021-01-04") #ymd("2020-10-05") 
last_forecast_date <- ymd("2021-06-07") ## roughly start of summer trough in MA
data_analysis_date <- as.Date("2022-03-14") ## could update with newer data

validation_forecast_dates <- seq.Date(first_forecast_date, last_forecast_date, by = "1 week")

## load hosp truth data
hosp_final <- load_truth(
  truth_source = "HealthData",
  target_variable = "inc hosp",
  locations = "25",
  data_location = "covidData",
  as_of = data_analysis_date) %>% 
  filter(
    ## only need data until last forecast date
    target_end_date <= last_forecast_date,
    ## data should start ~1wk prior to the first date, to allow for weekly differencing
    target_end_date >= data_start_date) %>%
  select(target_end_date, hosps = value) %>% 
  as_tsibble(index = target_end_date) %>% 
  tsibble::fill_gaps()

## load ReportCaseFinal data
report_case_final <- load_truth(
  truth_source = "JHU",
  target_variable = "inc case",
  temporal_resolution = "daily",
  locations = "25",
  data_location = "covidData",
  as_of = data_analysis_date) %>% 
  filter(
    target_end_date <= last_forecast_date, 
    target_end_date >= data_start_date, 
    ## note: filtering out values with <0 reported case values
    value >= 0) %>% 
  select(target_end_date, report_case_final = value) %>%  #change variable names
  as_tsibble(index = target_end_date) %>% 
  tsibble::fill_gaps()


## load TestCaseFinal data
test_case_final <- read_csv("csv-data/MA-DPH-covid-alldata.csv") %>%
  filter(issue_date == data_analysis_date) %>%
  mutate(target_end_date = test_date) %>% 
  filter(
    target_end_date <= last_forecast_date, 
    target_end_date >= data_start_date) %>% 
  select(target_end_date, test_case_final = new_positive, issue_date) %>% 
  mutate(test_case_final_smoothed = slider::slide_dbl(test_case_final, mean, .before = 6, .after=0, .complete=FALSE)) %>%
  as_tsibble(index = target_end_date) %>% 
  tsibble::fill_gaps() 


# set up variations on sarima specifications to consider in validation phase
sarima_variations <- expand.grid(p=0:3, d=0, P=0:3, D=0) # eventually will use d=0:1, D=0:1
## loop through forecast dates
for(forecast_date in validation_forecast_dates){
  
  ##  extract as-of data for TestCases 
  test_case_realtime <- read_csv("csv-data/MA-DPH-covid-alldata.csv") %>%  #chose latest issue date from csv files
    rename(target_end_date = test_date) %>%
    filter(issue_date == forecast_date,
           target_end_date <= forecast_date, 
           target_end_date >= data_start_date) %>% 
    select(target_end_date, test_case_realtime = new_positive) %>% 
    as_tsibble(index = target_end_date) %>% 
    tsibble::fill_gaps() 

  all_data <- hosp_final %>%
    left_join(report_case_final) %>%
    left_join(test_case_final) %>%
    left_join(test_case_realtime) %>%
    filter(target_end_date < forecast_date) %>%
    select(-issue_date)
    
  for (i in seq_len(nrow(sarima_variations))) {
    p <- sarima_variations$p[i]
    d <- sarima_variations$d[i]
    P <- sarima_variations$P[i]
    D <- sarima_variations$D[i]
    
    ## fit all models
    model1_solo <- arima_hosp_forecasts(all_data, p=p, d=d, P=P, D=D)
    # save model1_solo results
    
    model2_reportcasefinal <- arima_hosp_forecasts(all_data, case_col="report_case_final", case_p = 2, p=p, d=d, P=P, D=D)
    # save model2_reportcasefinal results
    
    model3a_testcasefinal <- arima_hosp_forecasts(all_data, case_col="test_case_final", case_p = 2, p=p, d=d, P=P, D=D)
    # save model3a_testcasefinal results
        
    model3b_testcaserealtime <- arima_hosp_forecasts(all_data, case_col="test_case_realtime", case_p = 2, p=p, d=d, P=P, D=D)
    # save model3b_testcaserealtime results
    
  }
}
