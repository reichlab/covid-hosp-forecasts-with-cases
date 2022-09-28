plot_as_of_case_data <- function(final_date = "2022-07-26",
                                 as_of_date = "2022-01-10",
                                 state) {
  ## preliminaries
  require(tidyverse)
  require(covidcast)
  require(RColorBrewer)
  require(cowplot)
  theme_set(theme_bw())
  
  ## data sources for all "final" data (no as-of data used in this analysis)
  ##  - test-date case data: from DPH files (see specific files below)
  ##  - report-date case data: from JHU CSSE, via covidcast
  ##  - trailing 7 day averages of case data: manually computed
  ##  - hospitalization data: from HealthData.gov, via covidcast
  
  ## latest files from each source
  ## - MA-DPH-csvdata-covid-2022-07-21.csv ** should be used
  ## - CA-DPH-testdate-covid-2022-07-26.csv ** should be used
  ## - CA-DPH-reportdate-covid-2022-07-26.csv
  ## - CA-JHU-reportdate-hospitalizations-2022-07-22.csv
  ## - CA-JHU-reportdate-cases-2022-07-22.csv
  
  ## check/update inputs
  state <- match.arg(state, c("ma", "ca"))
  state_code <- substr(covidcast::abbr_to_fips(state), 0,2)
  
  final_date <- as.Date(final_date)
  as_of_date <- as.Date(as_of_date)
  
  if(state=="ma" & as_of_date=="2022-01-17")
    as_of_date <- as.Date("2022-01-18")
  
  ## compile report-date case data from covidcast
  rptdate_final_data <- covidcast_signal(data_source = "jhu-csse",
                                         signal = "confirmed_incidence_num",
                                         start_day = "2020-03-01", end_day = final_date,
                                         geo_type = "state",
                                         geo_values = state) |> 
    rename(date = time_value, rpt_date_cases = value) |> 
    select(date, rpt_date_cases)
  
  rptdate_as_of_data <- covidcast_signal(data_source = "jhu-csse",
                                         signal = "confirmed_incidence_num",
                                         start_day = "2020-03-01", end_day = final_date,
                                         geo_type = "state",
                                         as_of = as_of_date,
                                         geo_values = state) |> 
    rename(date = time_value, rpt_date_cases = value) |> 
    select(date, rpt_date_cases)
  
  rptdate_final_smooth <- rptdate_final_data |> 
    arrange(date) |> 
    mutate(rpt_date_cases = (lag(rpt_date_cases, 0) + 
                               lag(rpt_date_cases, 1) + 
                               lag(rpt_date_cases, 2) + 
                               lag(rpt_date_cases, 3) + 
                               lag(rpt_date_cases, 4) + 
                               lag(rpt_date_cases, 5) + 
                               lag(rpt_date_cases, 6)) / 7 ) 

  rptdate_as_of_smooth <- rptdate_as_of_data |> 
    arrange(date) |> 
    mutate(rpt_date_cases = (lag(rpt_date_cases, 0) + 
                               lag(rpt_date_cases, 1) + 
                               lag(rpt_date_cases, 2) + 
                               lag(rpt_date_cases, 3) + 
                               lag(rpt_date_cases, 4) + 
                               lag(rpt_date_cases, 5) + 
                               lag(rpt_date_cases, 6)) / 7 ) 
  
  ## compile test-date case data from DPH files
  
  ## load state-specific testdate data
  if(state=="ca") {
    state_filename <- "CA-DPH-testdate-covid-"
    
    ## testdate final data
    testdate_final_filename <- file.path("csv-data", paste0(state_filename, final_date,".csv"))
    testdate_final_data <- read_csv(testdate_final_filename) |> 
      select(-issue_date) |> 
      rename(date = test_date, test_date_cases = new_positive)

    ## testdate as_of data
    testdate_as_of_filename <- file.path("csv-data", paste0(state_filename, as_of_date,".csv"))
    testdate_as_of_data <- read_csv(testdate_as_of_filename) |> 
      select(-issue_date) |> 
      rename(date = test_date, test_date_cases = new_positive)
    
  } else {
    state_filename <- "MA-DPH-csvdata-covid-"
    
    ## testdate final data
    testdate_final_filename <- file.path("csv-data", paste0(state_filename, final_date-5,".csv"))
    testdate_final_data <- read_csv(testdate_final_filename) |> 
      rename(date = test_date, test_date_cases = new_positive) |> 
      select(date, test_date_cases)
    
    ## testdate as_of data
    testdate_as_of_filename <- file.path("csv-data", paste0(state_filename, as_of_date,".csv"))
    testdate_as_of_data <- read_csv(testdate_as_of_filename) |> 
      rename(date = test_date, test_date_cases = new_positive) |> 
      select(date, test_date_cases)
  }
  
  
  
  testdate_final_smooth <- testdate_final_data |> 
    arrange(date) |> 
    mutate(test_date_cases = (lag(test_date_cases, 0) + 
                                lag(test_date_cases, 1) + 
                                lag(test_date_cases, 2) + 
                                lag(test_date_cases, 3) + 
                                lag(test_date_cases, 4) + 
                                lag(test_date_cases, 5) +
                                lag(test_date_cases, 6)) / 7 )

  testdate_as_of_smooth <- testdate_as_of_data |> 
    arrange(date) |> 
    mutate(test_date_cases = (lag(test_date_cases, 0) + 
                                lag(test_date_cases, 1) + 
                                lag(test_date_cases, 2) + 
                                lag(test_date_cases, 3) + 
                                lag(test_date_cases, 4) + 
                                lag(test_date_cases, 5) +
                                lag(test_date_cases, 6)) / 7 )
  
  ## merge test- and report-date case data together
  final_data <- left_join(testdate_final_data, rptdate_final_data) |> 
    pivot_longer(cols = -date, names_to = "data_type") |> 
    filter(date >= as_of_date-30, date <= as_of_date+30)
  
  final_data_smooth <- left_join(testdate_final_smooth, rptdate_final_smooth) |> 
    pivot_longer(cols = -date, names_to = "data_type") |> 
    filter(date >= as_of_date-30, date <= as_of_date+30)
  
  as_of_data <- left_join(testdate_as_of_data, rptdate_as_of_data) |> 
    pivot_longer(cols = -date, names_to = "data_type") |> 
    filter(date >= as_of_date-30, date <= as_of_date+30)

  as_of_data_smooth <- left_join(testdate_as_of_smooth, rptdate_as_of_smooth) |> 
    pivot_longer(cols = -date, names_to = "data_type") |> 
    filter(date >= as_of_date-30, date <= as_of_date+30)
  
  
  ## compile hosp data
  # final_hosp_data <- covidcast_signal(data_source="hhs",
  #                                     signal="confirmed_admissions_covid_1d",
  #                                     start_day = as_of_date - 30, end_day = as_of_date + 30,
  #                                     geo_type = "state",
  #                                     geo_values = state) |> 
  #   transmute(date = time_value, inc_hosp = value)
  # 
  # as_of_hosp_data <- covidcast_signal(data_source="hhs",
  #                                     signal="confirmed_admissions_covid_1d",
  #                                     start_day = as_of_date - 30, end_day = as_of_date + 30,
  #                                     geo_type = "state",
  #                                     as_of = as_of_date,
  #                                     geo_values = state) |> 
  #   transmute(date = time_value, inc_hosp = value)
  
  incomplete_data_window <- ifelse(state=="ma", 3.5, 7.5)
  
  p <- ggplot(mapping = aes(x=date)) +
    ## as of data lines
    geom_point(data = as_of_data, aes(y=value, color=data_type), alpha=.5) +
    ## final data lines 
    geom_point(data = final_data, aes(y=value, color=data_type), alpha=0.5, shape=1) +
    ## as_of smoothed data lines
    geom_line(data = as_of_data_smooth, aes(y=value, color=data_type), alpha=.6) +
    ## final smoothed data lines
    geom_line(data = final_data_smooth, aes(y=value, color=data_type), alpha=.6, linetype=2) +
    scale_x_date(NULL, 
                 limits = c(as_of_date - 30, as_of_date+30),
                 date_breaks = "1 month", 
                 date_minor_breaks = "1 week",
                 date_labels = "%b '%y",
                 expand = expansion(add=1)) +
    theme(axis.ticks.length.x = unit(0.5, "cm"), 
          axis.text.x = element_text(vjust = 7, hjust = -0.2),
          legend.position = c(0.03,0.97), legend.justification = c(0,1)) +
    ## draws rectangle shadowing the recent days of data
    annotate("rect", 
             xmin = as_of_date-incomplete_data_window, xmax=as_of_date,
             ymin = -Inf, ymax=Inf, alpha=.2) +
    ## draws vertical line at as_of_date
    geom_vline(xintercept = as_of_date, linetype=2, col="grey") + 
    scale_y_continuous(labels = scales::comma, name="incident cases") +
    scale_color_manual(labels = c("rpt_date_cases" = "report-date cases",
                                  "test_date_cases" = "test-date cases"),
                       breaks = c("rpt_date_cases", "test_date_cases"),
                       values = c("rpt_date_cases" = "#E7B800", 
                                  "test_date_cases" = "#FC4E07")) +
    scale_fill_manual(labels = c("rpt_date_cases" = "report-date cases",
                                 "test_date_cases" = "test-date cases"),
                      breaks = c("rpt_date_cases", "test_date_cases"),
                      values = c("rpt_date_cases" = "#E7B800", 
                                 "test_date_cases" = "#FC4E07")) +
    ## scale_color_brewer(palette = "Dark2") +
    ## scale_fill_brewer(palette = "Dark2") +
    ggtitle(paste(abbr_to_name(toupper(state)), "case data as of:", as_of_date))
  
  return(p)
}