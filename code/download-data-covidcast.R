library(tidyverse)
library(covidData)

# covidcast returns data as provided by the JHU/CSSE repository
# we download and save a snapshot of these data "as of" the date used for
# "finalized" data in our analysis: 2022-07-29
finalized_date <- "2022-07-22" ## date of "finalized" data to get from covidcast
analysis_start_date <- as.Date("2020-10-01")

for (state in c("ma", "ca")) {
  location_code <- unname(c("ma" = "25", "ca" = "06")[state])
  for (measure in c("cases", "hospitalizations")) {
    dat <- covidData::load_data(
      measure = measure,
      location_code = location_code,
      temporal_resolution = "daily",
      source = "covidcast",
      as_of = finalized_date) %>% 
      dplyr::filter(date >= analysis_start_date)
    
    filename <- paste0(toupper(state), "-JHU-reportdate-", measure, "-",
                       finalized_date, ".csv")
    write.csv(dat, file = file.path("csv-data", filename), row.names = FALSE)
  }
}
