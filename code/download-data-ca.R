## code for downloading CA data
## GitHub directory with lots of files: https://github.com/cagov/covid-static-data/tree/main/data/dashboard/confirmed-cases
## GitHub JSON file for all of CA: https://github.com/cagov/covid-static-data/blob/main/data/dashboard/confirmed-cases/california.json

## 2022-04-22: ONLY DOWNLOADING FINAL DATA, NOT WORRYING ABOUT VERIONS

library(jsonlite)
library(tidyverse)

download_ca_data <- function(json_link){
  ca_dat <- read_json(json_link, simplifyVector = TRUE)
  
  ca_test_date <- ca_dat$data$time_series$CONFIRMED_CASES_EPISODE_DATE$VALUES %>%
    rename(test_date = DATE,
           new_positive = VALUE) %>%
    mutate(issue_date = ca_dat$meta$PUBLISHED_DATE)
  
  ca_report_date <- ca_dat$data$time_series$CONFIRMED_CASES_REPORTED_DATE$VALUES %>%
    rename(report_date = DATE,
           new_positive = VALUE) %>%
    mutate(issue_date = ca_dat$meta$PUBLISHED_DATE)
  
  write_csv(ca_test_date, file = paste0("csv-data/CA-DPH-testdate-covid-", ca_dat$meta$PUBLISHED_DATE,".csv"))
  write_csv(ca_report_date, file = paste0("csv-data/CA-DPH-reportdate-covid-", ca_dat$meta$PUBLISHED_DATE,".csv"))
}

ca_json_links <- read_csv("csv-data/CA-json-links.csv") 

for (i in 1:nrow(ca_json_links)){
  download_ca_data(ca_json_links$file_link[i])
}
  