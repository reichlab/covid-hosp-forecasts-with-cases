library(tidyverse)
library(fabletools)
library(lubridate)

## date on which a model is run
forecast_monday <- ymd("2021-01-11")

## get hosp data through forecast_monday-1
hosp_data <- read_csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Hospitalizations.csv") %>%
  filter(location_name == "Massachusetts", 
         date < forecast_monday)

## get case data as of forecast_monday or most recent prior date
case_data <- read_csv("csv-data/MA-DPH-covid-alldata.csv") %>%
  mutate(test_date = as.Date(test_date))

most_recent_issue_date <- max(case_data$issue_date[case_data$issue_date <= forecast_monday])

case_data <- case_data %>%
  filter(issue_date == most_recent_issue_date) %>%
  as_tsibble(index = test_date)

## merge data

## set as tsibble

## run a simple model
fit <- case_dat %>%
  model(AR(total_positive ~ order(3)))