# figure 3 box plot
analysis_start_date <- as.Date("2020-10-01")
case_end_date <- as.Date("2022-04-28")

versioned_ma_test_cases <- read_csv("csv-data/MA-DPH-covid-alldata.csv") %>%
  dplyr::select(target_end_date = test_date, `test-date cases` = new_positive, as_of = issue_date) %>%
  dplyr::filter(target_end_date >= analysis_start_date, target_end_date <= case_end_date) 

all_issue_dates <- seq(as.Date("2021-01-01"), as.Date("2022-04-28"), by = 1)

versioned_ma_hosps <- purrr::map_dfr(
  all_issue_dates,
  function(x){
    covidData::load_data(
      measure = "hospitalizations",
      location_code = "25",
      temporal_resolution = "daily",
      source = "covidcast",
      as_of = x) %>% 
      dplyr::select(target_end_date = date, hospitalizations = inc) %>%
      dplyr::filter(target_end_date >= analysis_start_date)%>%
      dplyr::mutate(as_of = x)
  }
)

versioned_ma_report_cases <- purrr::map_dfr(
  all_issue_dates,
  function(x){
    covidData::load_data(
      measure = "cases",
      location_code = "25",
      temporal_resolution = "daily",
      source = "covidcast",
      as_of = x) %>% 
      dplyr::select(target_end_date = date, `report-date cases` = inc) %>%
      dplyr::filter(target_end_date >= analysis_start_date, target_end_date <= case_end_date )%>%
      dplyr::mutate(as_of = x)
  }
)

versioned_ma_all_data <- versioned_ma_test_cases %>%
  dplyr::full_join(versioned_ma_hosps, by = c("target_end_date" ="target_end_date", "as_of" = "as_of")) %>%
  dplyr::full_join(versioned_ma_report_cases, by = c("target_end_date" ="target_end_date", "as_of" = "as_of"))

most_recent_ma <- versioned_ma_all_data %>%
  filter(as_of == max(versioned_ma_all_data$as_of)) %>%
  tidyr::pivot_longer(c(`report-date cases`, `test-date cases`,`hospitalizations`), names_to = "Data", values_to ="value most recent")

all_ca_files <- list.files(pattern='CA-DPH-testdate-covid-', recursive=TRUE)

versioned_ca_test_cases <- purrr::map_dfr(
  all_ca_files,
  function(x){
  readr::read_csv(x) %>%
    dplyr::select(target_end_date = test_date, `test-date cases` = new_positive, as_of = issue_date) %>%
    dplyr::filter(target_end_date >= analysis_start_date, target_end_date <= case_end_date) 
})


versioned_ca_report_cases <- purrr::map_dfr(
  all_issue_dates,
  function(x){
    covidData::load_data(
      measure = "cases",
      location_code = "06",
      temporal_resolution = "daily",
      source = "covidcast",
      as_of = x) %>% 
      dplyr::select(target_end_date = date, `report-date cases` = inc) %>%
      dplyr::filter(target_end_date >= analysis_start_date, target_end_date <= case_end_date )%>%
      dplyr::mutate(as_of = x)
  }
)

versioned_ca_hosps <- purrr::map_dfr(
  all_issue_dates,
  function(x){
    covidData::load_data(
      measure = "hospitalizations",
      location_code = "06",
      temporal_resolution = "daily",
      source = "covidcast",
      as_of = x) %>% 
      dplyr::select(target_end_date = date, hospitalizations = inc) %>%
      dplyr::filter(target_end_date >= analysis_start_date)%>%
      dplyr::mutate(as_of = x)
  }
)

versioned_ca_all_data <- versioned_ca_test_cases %>%
  dplyr::full_join(versioned_ca_hosps, by = c("target_end_date" ="target_end_date", "as_of" = "as_of")) %>%
  dplyr::full_join(versioned_ca_report_cases, by = c("target_end_date" ="target_end_date", "as_of" = "as_of")) %>%

# most recent test date case data is issued on 2022-04-29
# the other two signals are issued on 2022-04-28
most_recent_ca <- versioned_ca_all_data %>%
  tidyr::pivot_longer(c(`report-date cases`, `test-date cases`,`hospitalizations`), names_to = "Data", values_to ="value most recent") %>%
  tidyr::drop_na() %>%
  dplyr::group_by(Data)%>%
  dplyr::filter(as_of ==max(as_of)) %>%
  ungroup()
    

date_with_lags <- versioned_ma_all_data %>%
  mutate(lag = as.numeric(as_of - target_end_date))%>%
  tidyr::pivot_longer(c(`report-date cases`, `test-date cases`,`hospitalizations`), names_to = "Data", values_to ="value on issue") %>%
  left_join(most_recent_ma, by=c("target_end_date" ="target_end_date","Data" = "Data")) %>%
  mutate(pct_of_most_recent = `value on issue`/`value most recent`,
         location = "Massachusetts")

date_with_lags_ca <- versioned_ca_all_data %>%
  mutate(lag = as.numeric(as_of - target_end_date))%>%
  tidyr::pivot_longer(c(`report-date cases`, `test-date cases`,`hospitalizations`), names_to = "Data", values_to ="value on issue") %>%
  left_join(most_recent_ca, by=c("target_end_date" ="target_end_date","Data" = "Data")) %>%
  mutate(pct_of_most_recent = `value on issue`/`value most recent`,
         location = "California")

date_with_lags <- date_with_lags %>%
  rbind(date_with_lags_ca) %>%
  dplyr::mutate(location = factor(location, levels = c("Massachusetts","California"))) %>%
  dplyr::mutate(Data = factor(Data, levels = c("test-date cases",
                                               "report-date cases",
                                               "hospitalizations") ))

sample_size <- date_with_lags %>% 
  group_by(lag, location, Data )%>%
  summarise(n = sum(!is.na(pct_of_most_recent)))

date_with_lags <- date_with_lags %>%
  dplyr::left_join(sample_size, by = c("lag" = "lag", "location" ="location", "Data" = "Data"))

# take out the boxplot with small sample size (lag=1, hosp for both locations)
min_sample_size<- min(date_with_lags[date_with_lags$lag <= 21,]$n)

date_with_lags <- date_with_lags %>%
  filter(lag <= 21) %>%
  mutate(small_sample_size = ifelse(n <= min_sample_size, TRUE, FALSE))

ggplot(date_with_lags %>% filter(small_sample_size == FALSE), aes(x=factor(lag), y=pct_of_most_recent)) +
  geom_boxplot(alpha=0.2, outlier.alpha = 0.1) +
  theme_bw()+
  coord_cartesian(xlim=c(0, 21)) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") + 
  facet_grid(Data~location, scales = "free")+
  ylab("Ratio of Reported Value on Issue to Final Reported Value")+
  xlab("Number of Days Between Issue Date and Event Date")+
  theme(legend.position="bottom")

ggsave('fig3.jpeg',dpi=300)
