## figure showing as-of and final data + forecasts
## Nick Reich, June 2022

plot_case_hosp_forecasts <- function(forecast_date, 
                                     final_date = "2022-07-26",
                                     include_ca_dph_reportcase = TRUE,
                                     window_around_forecast_date = 30,
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
    if(include_ca_dph_reportcase)
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
    arrange(date) |> 
    mutate(rpt_date_cases = (lag(rpt_date_cases, 0) + 
                               lag(rpt_date_cases, 1) + 
                               lag(rpt_date_cases, 2) + 
                               lag(rpt_date_cases, 3) + 
                               lag(rpt_date_cases, 4) + 
                               lag(rpt_date_cases, 5) + 
                               lag(rpt_date_cases, 6)) / 7 ) 
  
  ## for CA only, compile report-date DPH data and merge
  if(state=="ca" & include_ca_dph_reportcase) {
    state_filename <- "CA-DPH-reportdate-covid-"
    rptdate_dph_final_filename <- file.path("csv-data", paste0(state_filename, final_date,".csv"))
    rptdate_dph_final_data <- read_csv(rptdate_dph_final_filename) |> 
      select(-issue_date) |> 
      rename(date = report_date, rpt_date_dph_cases = new_positive)
    rptdate_final_data <- left_join(rptdate_final_data, rptdate_dph_final_data)
    rptdate_final_smooth <- rptdate_dph_final_data |> 
      arrange(date) |> 
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
    filter(date >= forecast_date-window_around_forecast_date, date <= forecast_date+window_around_forecast_date)
  
  final_data_smooth <- left_join(testdate_final_smooth, rptdate_final_smooth) |> 
    pivot_longer(cols = -date, names_to = "data_type") |> 
    filter(date >= forecast_date-window_around_forecast_date, date <= forecast_date+window_around_forecast_date)
  
  
  ## compile final hosp data
  final_hosp_data <- covidcast_signal(data_source="hhs",
                                      signal="confirmed_admissions_covid_1d",
                                      start_day = forecast_date - window_around_forecast_date, end_day = forecast_date + window_around_forecast_date,
                                      geo_type = "state",
                                      geo_values = state) |> 
    transmute(date = time_value, inc_hosp = value) |> 
    arrange(date) |> 
    mutate(inc_hosp_smooth = (lag(inc_hosp, 0) + 
                                lag(inc_hosp, 1) + 
                                lag(inc_hosp, 2) + 
                                lag(inc_hosp, 3) + 
                                lag(inc_hosp, 4) + 
                                lag(inc_hosp, 5) +
                                lag(inc_hosp, 6)) / 7 )
  
  
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
  
  if(state == "ca" & include_ca_dph_reportcase){
    rptdate_dph_fcast_filename <- file.path("forecasts", 
                                            state, 
                                            rptdate_dph_forecast_model,
                                            paste0(forecast_date, "-", rptdate_dph_forecast_model, ".csv"))
    rptdate_dph_forecast_data <- read_csv(rptdate_dph_fcast_filename) |> 
      mutate(target_variable = substr(target, start = nchar(target)-7, stop=nchar(target)),
             data_type = "rpt_date_dph_cases") |> 
      pivot_wider(names_from = quantile, values_from = value, names_prefix = "q")
  }
  
  ## construct conditional legends
  legend_labels_data <- if(include_ca_dph_reportcase){
    c("rpt_date_cases" = "report-date cases (CSSE)",
      "rpt_date_dph_cases" = "report-date cases (DPH)",
      "test_date_cases" = "test-date cases",
      "none" = "hospitalizations")
  } else {
    c("rpt_date_cases" = "report-date cases (CSSE)",
      "test_date_cases" = "test-date cases",
      "none" = "hospitalizations")
  }
  legend_labels_models <- if(include_ca_dph_reportcase){
    c("rpt_date_cases" = "ReportCase-CSSE",
      "rpt_date_dph_cases" = "ReportCase-DPH",
      "test_date_cases" = "TestCase",
      "none" = "HospOnly")
  } else {
    c("rpt_date_cases" = "ReportCase-CSSE",
      "test_date_cases" = "TestCase",
      "none" = "HospOnly")
  }
  
  legend_breaks <- if(include_ca_dph_reportcase) {
    c("rpt_date_cases", "rpt_date_dph_cases", "test_date_cases", "none")
  } else {
    c("rpt_date_cases", "test_date_cases", "none")
  }
  legend_colors_data <- if(include_ca_dph_reportcase){
    c("rpt_date_cases" = "#1f78b4",
      "rpt_date_dph_cases" = "#6a3d9a",
      "test_date_cases" = "#33a02c",
      "none" = "#ff7f00")
  } else {
    c("rpt_date_cases" = "#1f78b4",
      "test_date_cases" = "#33a02c",
      "none" = "#ff7f00")
  }
  legend_colors_models <- if(include_ca_dph_reportcase){
    c("rpt_date_cases" = "#a6cee3",
      "rpt_date_dph_cases" = "#cab2d6",
      "test_date_cases" = "#b2df8a",
      "none" = "#fdbf6f")
  } else {
    c("rpt_date_cases" = "#a6cee3",
      "test_date_cases" = "#b2df8a",
      "none" = "#fdbf6f")
  }
  
  ## plot cases
  case_plot <- 
    ggplot(mapping = aes(x=date)) +
    ## as of data lines
    # geom_point(data = as_of_data, aes(y=value), alpha=.5) +
    ## final data line until forecast date
    geom_point(data = filter(final_data), aes(y=value, color=data_type, shape=data_type), alpha=0.7) +
    ## final smoothed data line until forecast date
    geom_line(data = filter(final_data_smooth), aes(y=value, color=data_type)) +
    ## forecast ribbons
    geom_ribbon(data = filter(testdate_forecast_data, target_variable == "inc case"),
                aes(x=target_end_date, ymin=q0.1, ymax=q0.9), alpha=.3, size=0, fill="#33a02c") +
    geom_ribbon(data = filter(rptdate_forecast_data, target_variable == "inc case"),
                aes(x=target_end_date, ymin=q0.1, ymax=q0.9), alpha=.3, size=0, fill="#1f78b4") +
    ## forecast points
    geom_point(data = filter(testdate_forecast_data, target_variable == "inc case"),
              aes(x=target_end_date, y=q0.5, color=data_type), shape=5) +
    geom_point(data = filter(rptdate_forecast_data, target_variable == "inc case"),
              aes(x=target_end_date, y=q0.5, color=data_type), shape=2) +
    scale_x_date(NULL, 
                 limits = c(forecast_date - window_around_forecast_date, forecast_date+window_around_forecast_date),
                 date_breaks = "1 month", 
                 date_labels = "%b '%y",
                 expand = expansion(add=1)) +
    theme(axis.ticks.length.x = unit(0.5, "cm"), 
          axis.text.x = element_text(vjust = 7, hjust = -0.2), 
          legend.position = c(0.05,0.9), legend.justification = c(0,1), 
          legend.background=element_rect(fill = alpha("white", 0.5))) +
    geom_vline(xintercept = forecast_date+.5, linetype=2, col="grey") + 
    scale_y_continuous(labels = scales::comma, name="case counts") +
    scale_color_manual(name = "Data",
                       values = legend_colors_data,
                       breaks = legend_breaks,
                       labels = legend_labels_data, 
                       guide="none") +
    scale_shape_manual(name = "Data",
                       values = c("none" = 15,
                                  "rpt_date_dph_cases" = 16, 
                                  "rpt_date_cases" = 17, 
                                  "test_date_cases" = 18),
                       breaks = legend_breaks,
                       labels = legend_labels_data) +
    scale_fill_manual(name = "Data",
                      values = legend_colors_data,
                      breaks = legend_breaks,
                      labels = legend_labels_data,
                      guide = "none") +
    guides(shape = guide_legend(override.aes = list(color = legend_colors_data))) +
    ## scale_color_brewer(palette = "Dark2") +
    ## scale_fill_brewer(palette = "Dark2") +
    ggtitle(paste(abbr_to_name(toupper(state)), "case data and forecasts:", forecast_date))
  
  if(state=="ca" & include_ca_dph_reportcase) {
    case_plot <- case_plot +
      geom_point(data = filter(rptdate_dph_forecast_data, target_variable == "inc case"),
                aes(x=target_end_date, y=q0.5, color=data_type), shape = 1) +
      geom_ribbon(data = filter(rptdate_dph_forecast_data, target_variable == "inc case"),
                  aes(x=target_end_date, ymin=q0.1, ymax=q0.9), alpha=.3, size=0, fill="#6a3d9a")
  }
  
  ## plot hospitalizations
  hosp_plot <- ggplot(mapping = aes(x=date)) +
    #geom_line(data = as_of_hosp_data, aes(y=inc)) +
    #geom_line(data = filter(final_hosp_data, date <= forecast_date), aes(y=inc_hosp)) +
    ## draw hospitalization data
    geom_point(data = final_hosp_data, aes(y=inc_hosp), alpha=.8, shape=15, color="#ff7f00") +
    ## draw forecast ribbons
    geom_ribbon(data = filter(testdate_forecast_data, target_variable == "inc hosp"),
                aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill=data_type), alpha=.3, size=0) +
    geom_ribbon(data = filter(rptdate_forecast_data, target_variable == "inc hosp"),
                aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill=data_type), alpha=.3, size=0) +
    geom_ribbon(data = filter(none_forecast_data, target_variable == "inc hosp"),
                aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill="none"), alpha=.3, size=0) +
    ## draw forecast lines
    geom_point(data = filter(testdate_forecast_data, target_variable == "inc hosp"),
              aes(x=target_end_date, y=q0.5, color=data_type), shape=0) +
    geom_point(data = filter(rptdate_forecast_data, target_variable == "inc hosp"),
              aes(x=target_end_date, y=q0.5, color=data_type), shape=0) +
    geom_point(data = filter(none_forecast_data, target_variable == "inc hosp"),
              aes(x=target_end_date, y=q0.5, color="none"), shape=0) +
    scale_x_date(NULL, 
                 limits = c(forecast_date - window_around_forecast_date, forecast_date+window_around_forecast_date),
                 date_breaks = "1 month", 
                 date_labels = "%b '%y",
                 expand = expansion(add=1)) +
    theme(axis.ticks.length.x = unit(0.5, "cm"), 
          axis.text.x = element_text(vjust = 7, hjust = -0.2), 
          legend.position = c(0.05,0.9), legend.justification = c(0,1), 
          legend.background=element_rect(fill = alpha("white", 0.5))) +
    geom_vline(xintercept = forecast_date+.5, linetype=2, col="grey") + 
    scale_y_continuous(labels = scales::comma, name = "hospital admissions") +
    scale_color_manual(name = "Forecast model", 
                       labels = legend_labels_models,
                       breaks = legend_breaks,
                       values = legend_colors_data)+
    scale_fill_manual(name = "Forecast model",
                      labels = legend_labels_models,
                      breaks = legend_breaks,
                      values = legend_colors_data) +
    guides(shape = guide_legend(override.aes = list(color = legend_colors_data))) +
    ggtitle(paste(abbr_to_name(toupper(state)), "hospitalization data and forecasts:", forecast_date))
  
  if(state=="ca" & include_ca_dph_reportcase) {
    hosp_plot <- hosp_plot +
      geom_point(data = filter(rptdate_dph_forecast_data, target_variable == "inc hosp"),
                aes(x=target_end_date, y=q0.5), shape = 0, color="#6a3d9a") +
      geom_ribbon(data = filter(rptdate_dph_forecast_data, target_variable == "inc hosp"),
                  aes(x=target_end_date, ymin=q0.1, ymax=q0.9), alpha=.3, size=0, fill="#6a3d9a")
  }
  cowplot::plot_grid(case_plot, hosp_plot, nrow=2, align="v")
  
}

