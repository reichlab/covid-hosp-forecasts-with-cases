library(tidyverse)
library(lubridate)
library(fable)
library(covidHubUtils)
library(covidData)
library(GGally)

source("code/hosp_arima_forecast_function.R")

data_start_date <- ymd("2020-07-24")

## roughly start of winter wave in MA, also give us a few weeks of data to fit the model on early
forecast_date <- ymd("2021-05-03") 
data_analysis_date <- as.Date("2022-03-14") ## could update with newer data


## load hosp truth data
hosp_final <- load_truth(
  truth_source = "HealthData",
  target_variable = "inc hosp",
  locations = "25",
  data_location = "covidData",
  as_of = data_analysis_date) %>% 
  filter(
    ## only need data until last forecast date
    target_end_date <= forecast_date,
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
    target_end_date <= forecast_date, 
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
    target_end_date <= forecast_date, 
    target_end_date >= data_start_date) %>% 
  select(target_end_date, test_case_final = new_positive, issue_date) %>% 
  mutate(test_case_final_smoothed = slider::slide_dbl(test_case_final, mean, .before = 6, .after=0, .complete=FALSE)) %>%
  as_tsibble(index = target_end_date) %>% 
  tsibble::fill_gaps() 


test_case_realtime <- read_csv("csv-data/MA-DPH-covid-alldata.csv") %>% 
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
  mutate(test_case_final_diff = test_case_final - lag(test_case_final, 7)) %>%
  filter(target_end_date < forecast_date) %>%
  select(-issue_date)

## a suite of models
my_models <- all_data %>%
  ## it seemed better to transform the predictors as well
  mutate(test_case_final_smoothed = fourth_rt(test_case_final_smoothed),
         test_case_final = fourth_rt(test_case_final),
         report_case_final = fourth_rt(report_case_final)
         ) %>%
  add_lags("hosps", p=7, P=0) %>%
  add_lags("test_case_final_smoothed", p=7, P=0) %>%
  add_lags("test_case_final", p=7, P=0) %>%
  add_lags("report_case_final", p=7, P=0) %>%
  model(arima_.25 = ARIMA(fourth_rt_transformation(hosps) ~ pdq()),
        sarima_.25 = ARIMA(fourth_rt_transformation(hosps) ~ pdq() + PDQ()),
        hosp_lag1 = TSLM(fourth_rt_transformation(hosps) ~ hosps_lag1),
        hosp_lagx = TSLM(fourth_rt_transformation(hosps) ~ hosps_lag1 + hosps_lag2 + hosps_lag3 + hosps_lag4 + hosps_lag5 + hosps_lag6 + hosps_lag7),
        tcfs_lag1 = TSLM(fourth_rt_transformation(hosps) ~ test_case_final_smoothed_lag1),
        tcfs_lagx = TSLM(fourth_rt_transformation(hosps) ~ test_case_final_smoothed_lag1 + test_case_final_smoothed_lag2 + test_case_final_smoothed_lag3 + test_case_final_smoothed_lag4 + test_case_final_smoothed_lag5 + test_case_final_smoothed_lag6 + test_case_final_smoothed_lag7),
        tcf_lag1 = TSLM(fourth_rt_transformation(hosps) ~ test_case_final_lag1),
        tcf_lagx = TSLM(fourth_rt_transformation(hosps) ~ test_case_final_lag1 + test_case_final_lag2 + test_case_final_lag3 + test_case_final_lag4 + test_case_final_lag5 + test_case_final_lag6 + test_case_final_lag7),
        rcf_lag1 = TSLM(fourth_rt_transformation(hosps) ~ report_case_final_lag1),
        rcf_lagx = TSLM(fourth_rt_transformation(hosps) ~ report_case_final_lag1 + report_case_final_lag2 + report_case_final_lag3 + report_case_final_lag4 + report_case_final_lag5 + report_case_final_lag6 + report_case_final_lag7),
        nn_lagx = NNETAR(fourth_rt_transformation(hosps) ~ test_case_final_smoothed_lag1 + test_case_final_smoothed_lag2 + test_case_final_smoothed_lag3 + test_case_final_smoothed_lag4 + test_case_final_smoothed_lag5 + test_case_final_smoothed_lag6 + test_case_final_smoothed_lag7))

my_models %>% glance()


## an example single arima fit
arima_tcf <- arima_hosp_forecasts(all_data, case_col="test_case_final", case_p = 2, p=2, d=0, P=1, D=0)



## Evan's explorations
ggplot(data = all_data, mapping = aes(x = target_end_date, y = test_case_final)) +
  geom_line()

ggplot(data = all_data, mapping = aes(x = target_end_date, y = hosps)) +
  geom_line()

augmented_data <- all_data %>%
  mutate(test_case_final_smoothed = fourth_rt(test_case_final_smoothed),
         test_case_final = fourth_rt(test_case_final),
         report_case_final = fourth_rt(report_case_final)
         ) %>%
  add_lags("hosps", p=7, P=1) %>%
  add_lags("test_case_final_smoothed", p=7, P=1) %>%
  add_lags("test_case_final", p=7, P=1) %>%
  add_lags("report_case_final", p=7, P=1)

null_fit <- augmented_data %>%
  model(tslm_SAR11 = TSLM(fourth_rt_transformation(hosps) ~ fourth_rt(hosps_lag1) + fourth_rt(hosps_lag7) + fourth_rt(hosps_lag8)))

augmented_data$null_resid <- residuals(null_fit)[[".resid"]]

# look at relationship of residuals with lagged case values
ggpairs(
  augmented_data %>%
    dplyr::transmute(
      test_case_final_lag1, test_case_final_lag7, test_case_final_lag8,
      case_seasonal_diff = test_case_final_lag8 - test_case_final_lag1,
      null_resid)
)

# look at relationship of residuals with lagged smoothed case values
ggpairs(
  augmented_data %>%
    dplyr::transmute(
      test_case_final_lag1, test_case_final_smoothed_lag7, test_case_final_smoothed_lag8,
      case_seasonal_diff = test_case_final_smoothed_lag8 - test_case_final_smoothed_lag1,
      null_resid)
)

# preliminary plain-vanilla lm fits of resids ~ case_seasonal_diff
lm(null_resid ~ case_seasonal_diff,
  data = augmented_data %>%
    dplyr::mutate(
      case_seasonal_diff = test_case_final_lag8 - test_case_final_lag1))

lm(null_resid ~ case_seasonal_diff,
  data = augmented_data %>%
    dplyr::mutate(
      case_seasonal_diff = test_case_final_smoothed_lag8 - test_case_final_smoothed_lag1))

# using smoothed cases looks more promising from both plots and preliminary fits
# note that hosps were not transformed already, but cases were; hence, different treatment below
cases_fit <- augmented_data %>%
  model(tslm_SARX11 = TSLM(fourth_rt_transformation(hosps) ~ fourth_rt(hosps_lag1) + fourth_rt(hosps_lag7) + fourth_rt(hosps_lag8) +
                                                            test_case_final_smoothed_lag1 + test_case_final_smoothed_lag7 + test_case_final_smoothed_lag8))

# look at summary of null fit and cases fit
report(null_fit)
report(cases_fit)

# add these two to Nick's big list of models above
full_models <- dplyr::bind_cols(
  my_models,
  null_fit,
  cases_fit
)

glance(full_models)
