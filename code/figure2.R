# figure 2
library(dplyr)
library(ggplot2)
library(ggpubr)
library(broom)
library(forecast)
library(grid)


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


####  ccfs stratified by wave   ####

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


## supplemental analysis using differenced data

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
