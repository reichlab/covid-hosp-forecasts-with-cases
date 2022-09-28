## figure showing as-of and final data + forecasts
## Nick Reich, June 2022

plot_case_hosp_forecasts <- function(forecast_date, 
                                     final_date = "2022-07-26",
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
  
  forecast_date <- as.Date(forecast_date)
  final_date <- as.Date(final_date)
  
  ## chose best test phase models for each data type with smooth=FALSE
  if(state == "ca"){
    ## CA choices
    testdate_forecast_model <- "test_dph_smooth_case_True_SARIX_p_2_d_0_P_0_D_1"
    rptdate_forecast_model <- "report_jhu-csse_smooth_case_True_SARIX_p_4_d_1_P_0_D_0"
    rptdate_dph_forecast_model <- "report_dph_smooth_case_True_SARIX_p_4_d_1_P_0_D_0"
    none_forecast_model <- "none_jhu-csse_smooth_case_False_SARIX_p_1_d_1_P_1_D_0"
  } else {
    ## MA choices
    testdate_forecast_model <- "test_dph_smooth_case_True_SARIX_p_1_d_0_P_1_D_1"
    rptdate_forecast_model <- "report_jhu-csse_smooth_case_True_SARIX_p_4_d_1_P_0_D_0"
    none_forecast_model <- "none_jhu-csse_smooth_case_False_SARIX_p_1_d_0_P_1_D_1"
  }
  
  ## compile report-date case data from covidcast
  rptdate_final_data <- covidcast_signal(data_source = "jhu-csse",
                                         signal = "confirmed_incidence_num",
                                         start_day = "2020-03-01", end_day = final_date,
                                         geo_type = "state",
                                         geo_values = state) |> 
    rename(date = time_value, rpt_date_cases = value) |> 
    select(date, rpt_date_cases)
  
  rptdate_final_smooth <- rptdate_final_data |> 
    mutate(rpt_date_cases = (lag(rpt_date_cases, 0) + 
                               lag(rpt_date_cases, 1) + 
                               lag(rpt_date_cases, 2) + 
                               lag(rpt_date_cases, 3) + 
                               lag(rpt_date_cases, 4) + 
                               lag(rpt_date_cases, 5) + 
                               lag(rpt_date_cases, 6)) / 7 ) 
  
  ## for CA only, compile report-date DPH data and merge
  if(state=="ca") {
    state_filename <- "CA-DPH-reportdate-covid-"
    rptdate_dph_final_filename <- file.path("csv-data", paste0(state_filename, final_date,".csv"))
    rptdate_dph_final_data <- read_csv(rptdate_dph_final_filename) |> 
      select(-issue_date) |> 
      rename(date = report_date, rpt_date_dph_cases = new_positive)
    rptdate_final_data <- left_join(rptdate_final_data, rptdate_dph_final_data)
    rptdate_final_smooth <- rptdate_dph_final_data |> 
      mutate(rpt_date_dph_cases = (lag(rpt_date_dph_cases, 0) + 
                                     lag(rpt_date_dph_cases, 1) + 
                                     lag(rpt_date_dph_cases, 2) + 
                                     lag(rpt_date_dph_cases, 3) + 
                                     lag(rpt_date_dph_cases, 4) + 
                                     lag(rpt_date_dph_cases, 5) + 
                                     lag(rpt_date_dph_cases, 6)) / 7 ) |> 
      left_join(rptdate_final_smooth)
  }

  ## compile test-date case data from DPH files
  
  ## load state-specific testdate data
  if(state=="ca") {
    state_filename <- "CA-DPH-testdate-covid-"
    testdate_final_filename <- file.path("csv-data", paste0(state_filename, final_date,".csv"))
    testdate_final_data <- read_csv(testdate_final_filename) |> 
      select(-issue_date) |> 
      rename(date = test_date, test_date_cases = new_positive)
    
  } else {
    state_filename <- "MA-DPH-csvdata-covid-"
    testdate_final_filename <- file.path("csv-data", paste0(state_filename, final_date-5,".csv"))
    testdate_final_data <- read_csv(testdate_final_filename) |> 
      rename(date = test_date, test_date_cases = new_positive) |> 
      select(date, test_date_cases)
  }
  
  
  
  testdate_final_smooth <- testdate_final_data |> 
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
    filter(date >= forecast_date-30, date <= forecast_date+30)
  
  final_data_smooth <- left_join(testdate_final_smooth, rptdate_final_smooth) |> 
    pivot_longer(cols = -date, names_to = "data_type") |> 
    filter(date >= forecast_date-30, date <= forecast_date+30)
  
  
  ## compile final hosp data
  final_hosp_data <- covidcast_signal(data_source="hhs",
                                      signal="confirmed_admissions_covid_1d",
                                      start_day = forecast_date - 30, end_day = forecast_date + 30,
                                      geo_type = "state",
                                      geo_values = state) |> 
    transmute(date = time_value, inc_hosp = value)
  
  
  ## compile forecasts
  testdate_fcast_filename <- file.path("forecasts", 
                                       state, 
                                       testdate_forecast_model,
                                       paste0(forecast_date, "-", testdate_forecast_model, ".csv"))
  rptdate_fcast_filename <- file.path("forecasts", 
                                      state, 
                                      rptdate_forecast_model,
                                      paste0(forecast_date, "-", rptdate_forecast_model, ".csv"))
  none_fcast_filename <- file.path("forecasts", 
                                   state, 
                                   none_forecast_model,
                                   paste0(forecast_date, "-", none_forecast_model, ".csv"))
  
  testdate_forecast_data <- read_csv(testdate_fcast_filename) |> 
    mutate(target_variable = substr(target, start = nchar(target)-7, stop=nchar(target)),
           data_type = "test_date_cases") |> 
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "q")
  rptdate_forecast_data <- read_csv(rptdate_fcast_filename) |> 
    mutate(target_variable = substr(target, start = nchar(target)-7, stop=nchar(target)),
           data_type = "rpt_date_cases") |> 
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "q")
  none_forecast_data <- read_csv(none_fcast_filename) |> 
    mutate(target_variable = substr(target, start = nchar(target)-7, stop=nchar(target)),
           data_type = "none") |> 
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "q")
  
  if(state == "ca"){
    rptdate_dph_fcast_filename <- file.path("forecasts", 
                                            state, 
                                            rptdate_dph_forecast_model,
                                            paste0(forecast_date, "-", rptdate_dph_forecast_model, ".csv"))
    rptdate_dph_forecast_data <- read_csv(rptdate_dph_fcast_filename) |> 
      mutate(target_variable = substr(target, start = nchar(target)-7, stop=nchar(target)),
             data_type = "rpt_date_dph_cases") |> 
      pivot_wider(names_from = quantile, values_from = value, names_prefix = "q")
  }
  
  ## plot cases
  case_plot <- 
    ggplot(mapping = aes(x=date)) +
    ## as of data lines
    # geom_point(data = as_of_data, aes(y=value), alpha=.5) +
    ## final data line until forecast date
    geom_point(data = filter(final_data), aes(y=value, color=data_type, shape=data_type), alpha=0.5) +
    ## final smoothed data line until forecast date
    geom_line(data = filter(final_data_smooth, date<=forecast_date), aes(y=value, color=data_type), alpha=.6) +
    ## forecast ribbons
    geom_ribbon(data = filter(testdate_forecast_data, target_variable == "inc case"),
                aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill=data_type), alpha=.3, size=0) +
    geom_ribbon(data = filter(rptdate_forecast_data, target_variable == "inc case"),
                aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill=data_type), alpha=.3, size=0) +
    ## forecast lines
    geom_line(data = filter(testdate_forecast_data, target_variable == "inc case"),
              aes(x=target_end_date, y=q0.5, color=data_type), alpha=.7, size=1.5) +
    geom_line(data = filter(rptdate_forecast_data, target_variable == "inc case"),
              aes(x=target_end_date, y=q0.5, color=data_type),alpha=.7, size=1.5) +
    scale_x_date(NULL, 
                 limits = c(forecast_date - 30, forecast_date+30),
                 date_breaks = "1 month", 
                 date_labels = "%b '%y",
                 expand = expansion(add=1)) +
    theme(axis.ticks.length.x = unit(0.5, "cm"), 
          axis.text.x = element_text(vjust = 7, hjust = -0.2),
          legend.position = "none") +
    geom_vline(xintercept = forecast_date+.5, linetype=2, col="grey") + 
    scale_y_continuous(labels = scales::comma, name="incident cases") +
    scale_color_manual(values = c("rpt_date_cases" = "#E7B800", 
                                  "rpt_date_dph_cases" = "#778A35",
                                  "test_date_cases" = "#FC4E07")) +
    scale_shape_manual(values = c("rpt_date_dph_cases" = 16, 
                                  "rpt_date_cases" = 17, 
                                  "test_date_cases" = 18)) +
    scale_fill_manual(values = c("rpt_date_cases" = "#E7B800", 
                                 "rpt_date_dph_cases" = "#778A35",
                                 "test_date_cases" = "#FC4E07")) +
    ## scale_color_brewer(palette = "Dark2") +
    ## scale_fill_brewer(palette = "Dark2") +
    ggtitle(paste(abbr_to_name(toupper(state)), "case data and forecasts:", forecast_date))
  
  if(state=="ca") {
    case_plot <- case_plot +
      geom_line(data = filter(rptdate_dph_forecast_data, target_variable == "inc case"),
                aes(x=target_end_date, y=q0.5, color=data_type), alpha=.7, size=1.5) +
      geom_ribbon(data = filter(rptdate_dph_forecast_data, target_variable == "inc case"),
                  aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill=data_type), alpha=.3, size=0)
  }
  
  ## plot hospitalizations
  hosp_plot <- ggplot(mapping = aes(x=date)) +
    #geom_line(data = as_of_hosp_data, aes(y=inc)) +
    geom_line(data = filter(final_hosp_data, date <= forecast_date), aes(y=inc_hosp)) +
    geom_line(data = final_hosp_data, aes(y=inc_hosp), alpha=.2) +
    ## draw forecast ribbons
    geom_ribbon(data = filter(testdate_forecast_data, target_variable == "inc hosp"),
                aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill=data_type), alpha=.3, size=0) +
    geom_ribbon(data = filter(rptdate_forecast_data, target_variable == "inc hosp"),
                aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill=data_type), alpha=.3, size=0) +
    geom_ribbon(data = filter(none_forecast_data, target_variable == "inc hosp"),
                aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill="none"), alpha=.3, size=0) +
    ## draw forecast lines
    geom_line(data = filter(testdate_forecast_data, target_variable == "inc hosp"),
              aes(x=target_end_date, y=q0.5, color=data_type), size=1.5, alpha=.7) +
    geom_line(data = filter(rptdate_forecast_data, target_variable == "inc hosp"),
              aes(x=target_end_date, y=q0.5, color=data_type), size=1.5, alpha=.7) +
    geom_line(data = filter(none_forecast_data, target_variable == "inc hosp"),
              aes(x=target_end_date, y=q0.5, color="none"), size=2, alpha=.7) +
    scale_x_date(NULL, 
                 limits = c(forecast_date - 30, forecast_date+30),
                 date_breaks = "1 month", 
                 date_labels = "%b '%y",
                 expand = expansion(add=1)) +
    theme(axis.ticks.length.x = unit(0.5, "cm"), 
          axis.text.x = element_text(vjust = 7, hjust = -0.2), 
          legend.position = c(0.05,0.9), legend.justification = c(0,1), 
          legend.background=element_rect(fill = alpha("white", 0.5))) +
    geom_vline(xintercept = forecast_date+.5, linetype=2, col="grey") + 
    scale_y_continuous(labels = scales::comma, name = "incident hospitalizations") +
    scale_color_manual(name = "Models", 
                       labels = c("rpt_date_cases" = "ReportCase-CSSE",
                                  "rpt_date_dph_cases" = "ReportCase-DPH",
                                  "test_date_cases" = "TestCase",
                                  "none" = "HospOnly"),
                       breaks = c("rpt_date_cases", "rpt_date_dph_cases", "test_date_cases", "none"),
                       values = c("rpt_date_cases" = "#E7B800", 
                                  "rpt_date_dph_cases" = "#778A35",
                                  "test_date_cases" = "#FC4E07", 
                                  "none" = "darkgrey")) +
    scale_fill_manual(name = "Models",
                      labels = c("rpt_date_cases" = "ReportCase-CSSE",
                                 "rpt_date_dph_cases" = "ReportCase-DPH",
                                 "test_date_cases" = "TestCase",
                                 "none" = "HospOnly"),
                      breaks = c("rpt_date_cases", "rpt_date_dph_cases", "test_date_cases", "none"),
                      values = c("rpt_date_cases" = "#E7B800", 
                                 "rpt_date_dph_cases" = "#778A35",
                                 "test_date_cases" = "#FC4E07", 
                                 "none" = "darkgrey")) +
    ggtitle(paste(abbr_to_name(toupper(state)), "hospitalization data and forecasts:", forecast_date))
  
  if(state=="ca") {
    hosp_plot <- hosp_plot +
      geom_line(data = filter(rptdate_dph_forecast_data, target_variable == "inc hosp"),
                aes(x=target_end_date, y=q0.5, color=data_type), size=1.5, alpha=.7) +
      geom_ribbon(data = filter(rptdate_dph_forecast_data, target_variable == "inc hosp"),
                  aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill=data_type), alpha=.3, size=0)
  }
  cowplot::plot_grid(case_plot, hosp_plot, nrow=2, align="v")
  
}

