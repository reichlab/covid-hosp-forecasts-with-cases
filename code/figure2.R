# figure 2
library(dplyr)
library(ggplot2)
library(ggpubr)
library(broom)
library(forecast)
library(grid)

theme_set(theme_bw())

# use all data as of 2022-07-22
as_of_date <- as.Date("2022-07-22")
last_forecast_date <- as.Date("2022-07-18") ## monday prior to as-of-date
analysis_start_date <- as.Date("2020-10-01")
validation_date <- as.Date("2020-12-07")
validation_test_split <- as.Date("2021-06-07")

ma_case_test <- readr::read_csv("csv-data/combined_data_ma_test_dph.csv") %>%
  dplyr::filter(forecast_date == last_forecast_date) %>%
  dplyr::select(target_end_date = date, 
                `test-date cases` = case,
                `smooth DPH test-date cases` = corrected_case_7da,
                hospitalizations = hosp)
ma_case_report <- readr::read_csv("csv-data/combined_data_ma_report_jhu-csse.csv") %>%
  dplyr::filter(forecast_date == last_forecast_date) %>%
  dplyr::select(target_end_date = date, 
                `report-date cases` = case,
                `smooth JHU report-date cases` = corrected_case_7da)

ca_case_test <- readr::read_csv("csv-data/combined_data_ca_test_dph.csv") %>%
  dplyr::filter(forecast_date == last_forecast_date) %>%
  dplyr::select(target_end_date = date, 
                `test-date cases` = case,
                `smooth DPH test-date cases` = corrected_case_7da,
                hospitalizations = hosp)
ca_case_report_jhu <- readr::read_csv("csv-data/combined_data_ca_report_jhu-csse.csv") %>%
  dplyr::filter(forecast_date == last_forecast_date) %>%
  dplyr::select(target_end_date = date, 
                `report-date cases` = case,
                `smooth JHU report-date cases` = corrected_case_7da)
ca_case_report_dph <- readr::read_csv("csv-data/combined_data_ca_report_dph.csv") %>%
  dplyr::filter(forecast_date == last_forecast_date) %>%
  dplyr::select(target_end_date = date, 
                `CA DPH report-date cases` = case,
                `smooth CA DPH report-date cases` = corrected_case_7da)


#### figure for ccfs with smooth data, entire dataset ####
ma_data <- ma_case_report %>%
  dplyr::left_join(ma_case_test, by = "target_end_date")

ma_case_report_hosp_ccf <- broom::tidy(ccf(ma_data$`smooth JHU report-date cases`,
                                           ma_data$hospitalizations,
                                           na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth JHU report-date cases` = acf)

ma_case_test_hosp_ccf <- broom::tidy(ccf(ma_data$`smooth DPH test-date cases`, 
                                         ma_data$hospitalizations,
                                         na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth DPH test-date cases` = acf)

ma_ccf <- dplyr::full_join(ma_case_report_hosp_ccf, ma_case_test_hosp_ccf, by = "lag") %>%
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(location = "Massachusetts")

ca_data <- ca_case_report_jhu %>%
  dplyr::left_join(ca_case_test, by = "target_end_date") %>%
  dplyr::left_join(ca_case_report_dph, by = "target_end_date")

ca_case_report_hosp_ccf <- broom::tidy(ccf(ca_data$`smooth JHU report-date cases`, 
                                           ca_data$hospitalizations,
                                           na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth JHU report-date cases` = acf)

ca_dph_case_report_hosp_ccf <- broom::tidy(ccf(ca_data$`smooth CA DPH report-date cases`, 
                                           ca_data$hospitalizations,
                                           na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth CA DPH report-date cases` = acf)

ca_case_test_hosp_ccf <- broom::tidy(ccf(ca_data$`smooth DPH test-date cases`, 
                                         ca_data$hospitalizations,
                                         na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth DPH test-date cases` = acf)

ca_ccf <- dplyr::full_join(ca_case_report_hosp_ccf, ca_case_test_hosp_ccf, by = "lag") %>%
  dplyr::full_join(ca_dph_case_report_hosp_ccf, by = "lag")%>%
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(location = "California")

both_ccf <- bind_rows(ca_ccf, ma_ccf) %>%
  mutate(location = factor(location, levels = c("Massachusetts", "California")))

vertical_line_layer <- both_ccf %>%
  dplyr::group_by(location, Data) %>%
  dplyr::summarize(xint = lag[value == max(value)],
                   max_ccf = max(value)) %>%
  dplyr::ungroup()

p2 <- ggplot(data = both_ccf, 
       aes(x = lag, y = value, color = Data)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(name = "Case Data Source",
                     values = c("smooth DPH test-date cases" = "#33a02c",
                                "smooth JHU report-date cases" = "#1f78b4", 
                                "smooth CA DPH report-date cases" = "#6a3d9a"))+
  geom_vline(aes(xintercept=0), linetype="solid", color = "darkgrey")+
  geom_segment(aes(x=xint, y = 0, xend= xint, yend = max_ccf), 
                data=vertical_line_layer, linetype="dashed")+
  ylab("Cross-correlation between hospitalizations and case data source")+
  xlab("lag, in days")+
  ylim(0,1)+
  scale_x_continuous(breaks = c(-25, -20,-10, 0, 10, 20, 25,
                                vertical_line_layer$xint),
                     minor_breaks = NULL)+
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        plot.margin = unit(c(2, 5, 1, 5), "pt")) +
  facet_wrap(location ~., scales = "free_y", nrow = 2) 

ggsave('figures/fig2_smooth_ccfs.jpeg',dpi=300, width=9, height=8)


####  ccfs stratified by phase   ####

ma_data_split <- ma_data |> 
  mutate(target_end_date_period = ifelse(target_end_date <= validation_test_split, 
                                         "validation", "test"))

## compute ccfs on test and validation splits for report-date cases
ma_case_report_hosp_ccf_valid <- ma_data_split |> 
  filter(target_end_date_period == "validation") |> 
  with(ccf(`smooth JHU report-date cases`,
           hospitalizations,
           na.action=na.omit, plot = FALSE)) |> 
  broom::tidy() |> 
  dplyr::rename(`smooth JHU report-date cases` = acf) |> 
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") |> 
  mutate(period = "validation") 

ma_case_report_hosp_ccf_test <- ma_data_split |> 
  filter(target_end_date_period == "test") |> 
  with(ccf(`smooth JHU report-date cases`,
           hospitalizations,
           na.action=na.omit, plot = FALSE)) |> 
  broom::tidy() |> 
  dplyr::rename(`smooth JHU report-date cases` = acf) |> 
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") |> 
  mutate(period = "test")

## compute ccfs on test and validation splits for test-date cases
ma_case_test_hosp_ccf_valid <-  ma_data_split |> 
  filter(target_end_date_period == "validation") |> 
  with(ccf(`smooth DPH test-date cases`,
           hospitalizations,
           na.action=na.omit, plot = FALSE)) |> 
  broom::tidy() |> 
  dplyr::rename(`smooth DPH test-date cases` = acf) |> 
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") |> 
  mutate(period = "validation")

ma_case_test_hosp_ccf_test <-  ma_data_split |> 
  filter(target_end_date_period == "test") |> 
  with(ccf(`smooth DPH test-date cases`,
           hospitalizations,
           na.action=na.omit, plot = FALSE)) |> 
  broom::tidy() |> 
  dplyr::rename(`smooth DPH test-date cases` = acf) |> 
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") |> 
  mutate(period = "test")


ma_ccf <- dplyr::bind_rows(ma_case_report_hosp_ccf_valid, 
                           ma_case_report_hosp_ccf_test, 
                           ma_case_test_hosp_ccf_valid, 
                           ma_case_test_hosp_ccf_test) |> 
  dplyr::mutate(location = "Massachusetts")
## table was manually inspected for max correlations

ggplot(data = ma_ccf, 
       aes(x = lag, y = value, color = Data)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(name = "Case Data Source",
                     values = c("smooth DPH test-date cases" = "#33a02c",
                                "smooth JHU report-date cases" = "#1f78b4", 
                                "smooth CA DPH report-date cases" = "#6a3d9a"))+
  ylab("Cross-correlation between hospitalizations and case data source")+
  xlab("lag, in days")+
  ylim(0,1)+
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        plot.margin = unit(c(2, 5, 1, 5), "pt")) +
  facet_wrap(period ~., scales = "free_y", nrow = 2) 

## and for CA

ca_data_split <- ca_data |> 
  mutate(target_end_date_period = ifelse(target_end_date <= validation_test_split, 
                                         "validation", "test"))

## compute ccfs on test and validation splits for report-date cases
ca_case_report_hosp_ccf_valid <- ca_data_split |> 
  filter(target_end_date_period == "validation") |> 
  with(ccf(`smooth JHU report-date cases`,
           hospitalizations,
           na.action=na.omit, plot = FALSE)) |> 
  broom::tidy() |> 
  dplyr::rename(`smooth JHU report-date cases` = acf) |> 
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") |> 
  mutate(period = "validation") 

ca_case_report_hosp_ccf_test <- ca_data_split |> 
  filter(target_end_date_period == "test") |> 
  with(ccf(`smooth JHU report-date cases`,
           hospitalizations,
           na.action=na.omit, plot = FALSE)) |> 
  broom::tidy() |> 
  dplyr::rename(`smooth JHU report-date cases` = acf) |> 
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") |> 
  mutate(period = "test")

## compute ccfs on test and validation splits for test-date cases
ca_case_test_hosp_ccf_valid <-  ca_data_split |> 
  filter(target_end_date_period == "validation") |> 
  with(ccf(`smooth DPH test-date cases`,
           hospitalizations,
           na.action=na.omit, plot = FALSE)) |> 
  broom::tidy() |> 
  dplyr::rename(`smooth DPH test-date cases` = acf) |> 
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") |> 
  mutate(period = "validation")

ca_case_test_hosp_ccf_test <-  ca_data_split |> 
  filter(target_end_date_period == "test") |> 
  with(ccf(`smooth DPH test-date cases`,
           hospitalizations,
           na.action=na.omit, plot = FALSE)) |> 
  broom::tidy() |> 
  dplyr::rename(`smooth DPH test-date cases` = acf) |> 
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") |> 
  mutate(period = "test")


ca_ccf <- dplyr::bind_rows(ca_case_report_hosp_ccf_valid, 
                           ca_case_report_hosp_ccf_test, 
                           ca_case_test_hosp_ccf_valid, 
                           ca_case_test_hosp_ccf_test) |> 
  dplyr::mutate(location = "California")
## table was manually inspected for max correlations

## plot the case/hosp ratios

p1 <- ggplot(ca_data_split, aes(x=target_end_date, 
                          y=`smooth JHU report-date cases`/hospitalizations, 
                          color=target_end_date_period, 
                          shape = target_end_date_period)) + 
  geom_point() + 
  ggtitle("A: CA JHU report-date cases to hosps ratio") +
  xlab(NULL) + ylab("# reported cases per hospitalization") +
  geom_vline(aes(xintercept = validation_test_split)) +
  theme(legend.position = "none")

p2 <- ggplot(ma_data_split, aes(x=target_end_date, 
                          y=`smooth JHU report-date cases`/hospitalizations, 
                          color=target_end_date_period, 
                          shape = target_end_date_period)) + 
  geom_point() + 
  ggtitle("C: MA JHU report-date cases to hosps ratio") +
  xlab(NULL) + ylab("# reported cases per hospitalization") +
  geom_vline(aes(xintercept = validation_test_split)) +
  theme(legend.position = "none")

p3 <- ggplot(ca_data_split, aes(x=`smooth JHU report-date cases`, 
                          y=hospitalizations, 
                          color=target_end_date_period, 
                          shape = target_end_date_period,
                          linetype = target_end_date_period)) + 
  ggtitle("B: CA correlation of cases and hospitalization") +
  geom_point(alpha=.3) +
  scale_x_log10() + scale_y_log10() + 
  geom_smooth(method="lm", se=FALSE) +
  theme(legend.position = "none")

p4 <- ggplot(ma_data_split, aes(x=`smooth JHU report-date cases`, 
                          y=hospitalizations, 
                          color=target_end_date_period, 
                          shape = target_end_date_period,
                          linetype = target_end_date_period)) + 
  geom_point(alpha=.3) +
  ggtitle("D: MA correlation of cases and hospitalization") +
  scale_x_log10() + scale_y_log10() + 
  geom_smooth(method="lm", se=FALSE) +
  theme(legend.position = c(0.9,0.1), 
        legend.justification = c(0.9,0.1),
        legend.title = element_blank())


p_all <- gridExtra::grid.arrange(p1, p3, p2, p4, nrow=2)
ggsave('figures/supp_cases_v_hosps.pdf', plot = p_all, width=9, height=6)


#### supplemental analysis using differenced data ####

ma_case_test_diff <- ma_case_test %>%
  mutate(`test-date cases` = `test-date cases` - lag(`test-date cases`, 1),
         `smooth DPH test-date cases` = `smooth DPH test-date cases` - lag(`smooth DPH test-date cases`, 1),
         hospitalizations = hospitalizations - lag(hospitalizations, 1))

ma_case_report_diff <- ma_case_report %>%
  mutate(`report-date cases` = `report-date cases` - lag(`report-date cases`, 1),
         `smooth JHU report-date cases` = `smooth JHU report-date cases` - lag(`smooth JHU report-date cases`,1))

ca_case_test_diff <- ca_case_test %>%
  mutate(`test-date cases` = `test-date cases` - lag(`test-date cases`, 1),
         `smooth DPH test-date cases` = `smooth DPH test-date cases` - lag(`smooth DPH test-date cases`, 1),
         hospitalizations = hospitalizations - lag(hospitalizations, 1))

ca_case_report_jhu_diff <- ca_case_report_jhu %>%
  mutate(`report-date cases` = `report-date cases` - lag(`report-date cases`, 1),
         `smooth JHU report-date cases` = `smooth JHU report-date cases` - lag(`smooth JHU report-date cases`,1))

ca_case_report_dph_diff <- ca_case_report_dph %>%
  mutate(`CA DPH report-date cases` = `CA DPH report-date cases` - lag(`CA DPH report-date cases`, 1),
         `smooth CA DPH report-date cases` = `smooth CA DPH report-date cases` - lag(`smooth CA DPH report-date cases`, 1))

## make plot, code copied/modified from above
ma_data <- ma_case_report_diff %>%
  dplyr::left_join(ma_case_test_diff, by = "target_end_date")

ma_case_report_hosp_ccf <- broom::tidy(ccf(ma_data$`smooth JHU report-date cases`,
                                           ma_data$hospitalizations,
                                           na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth JHU report-date cases` = acf)

ma_case_test_hosp_ccf <- broom::tidy(ccf(ma_data$`smooth DPH test-date cases`, 
                                         ma_data$hospitalizations,
                                         na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth DPH test-date cases` = acf)

ma_ccf <- dplyr::full_join(ma_case_report_hosp_ccf, ma_case_test_hosp_ccf, by = "lag") %>%
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(location = "Massachusetts")

ca_data <- ca_case_report_jhu_diff %>%
  dplyr::left_join(ca_case_test_diff, by = "target_end_date") %>%
  dplyr::left_join(ca_case_report_dph_diff, by = "target_end_date")

ca_case_report_hosp_ccf <- broom::tidy(ccf(ca_data$`smooth JHU report-date cases`, 
                                           ca_data$hospitalizations,
                                           na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth JHU report-date cases` = acf)

ca_dph_case_report_hosp_ccf <- broom::tidy(ccf(ca_data$`smooth CA DPH report-date cases`, 
                                               ca_data$hospitalizations,
                                               na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth CA DPH report-date cases` = acf)

ca_case_test_hosp_ccf <- broom::tidy(ccf(ca_data$`smooth DPH test-date cases`, 
                                         ca_data$hospitalizations,
                                         na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth DPH test-date cases` = acf)

ca_ccf <- dplyr::full_join(ca_case_report_hosp_ccf, ca_case_test_hosp_ccf, by = "lag") %>%
  dplyr::full_join(ca_dph_case_report_hosp_ccf, by = "lag")%>%
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(location = "California")

both_ccf <- bind_rows(ca_ccf, ma_ccf) %>%
  mutate(location = factor(location, levels = c("Massachusetts", "California")))

vertical_line_layer <- both_ccf %>%
  dplyr::group_by(location, Data) %>%
  dplyr::summarize(xint = lag[value == max(value)],
                   max_ccf = max(value)) %>%
  dplyr::ungroup()

ggplot(data = both_ccf, 
       aes(x = lag, y = value, color = Data)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(name = "Case Data Source",
                     values = c("smooth DPH test-date cases" = "#33a02c",
                                "smooth JHU report-date cases" = "#1f78b4", 
                                "smooth CA DPH report-date cases" = "#6a3d9a"))+
  geom_vline(aes(xintercept=0), linetype="solid", color = "darkgrey")+
  geom_segment(aes(x=xint, y = 0, xend= xint, yend = max_ccf), 
               data=vertical_line_layer, linetype="dashed")+
  ylab("Cross-correlation between hospitalizations and case data source")+
  xlab("lag, in days")+
  scale_x_continuous(breaks = c(-25, -20,-10, 0, 10, 20, 25,
                                vertical_line_layer$xint),
                     minor_breaks = NULL)+
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        plot.margin = unit(c(2, 5, 1, 5), "pt")) +
  facet_wrap(location ~., scales = "free_y", nrow = 2) 
