# figure 2
library(dplyr)
library(covidHubUtils)
library(ggplot2)
library(ggpubr)
library(forecast)
library(broom)
library(cowplot)
library(grid)

# use all data as of 2022-04-29
as_of_date <- as.Date("2022-04-29")
analysis_start_date <- as.Date("2020-10-01")
hosp_start_date <- as.Date("2020-08-01")
case_start_date <- as.Date("2020-03-01")
case_end_date <- as.Date("2022-04-28")
validation_test_split <- as.Date("2021-06-07")

ma_case_test <- readr::read_csv("csv-data/MA-DPH-csvdata-covid-2022-04-28.csv") %>%
  dplyr::select(target_end_date = test_date, `test-date cases` = new_positive) %>%
  dplyr::filter(target_end_date >= analysis_start_date, target_end_date <= case_end_date) 
ca_case_test <- readr::read_csv("csv-data/CA-DPH-testdate-covid-2022-04-29.csv") %>%
  dplyr::select(target_end_date = test_date, `test-date cases` = new_positive) %>%
  dplyr::filter(target_end_date >= analysis_start_date, target_end_date <= case_end_date)
ma_case_report <- covidData::load_data(
  measure = "cases",
  location_code = "25",
  temporal_resolution = "daily",
  source = "covidcast",
  as_of = as_of_date) %>% 
  dplyr::select(target_end_date = date, `report-date cases` = inc) %>%
  dplyr::filter(target_end_date >= analysis_start_date, target_end_date <= case_end_date) 
ca_case_report <- covidData::load_data(
  measure = "cases",
  location_code = "06",
  temporal_resolution = "daily",
  source = "covidcast",
  as_of = as_of_date) %>% 
  dplyr::select(target_end_date = date, `report-date cases` = inc) %>%
  dplyr::filter(target_end_date >= analysis_start_date, target_end_date <= case_end_date) 
ca_dph_case_report <- readr::read_csv("csv-data/CA-DPH-reportdate-covid-2022-04-29.csv") %>%
  dplyr::select(target_end_date = report_date, `CA DPH report-date cases` = new_positive) %>%
ma_hosps <- covidData::load_data(
  measure = "hospitalizations",
  location_code = "25",
  temporal_resolution = "daily",
  source = "covidcast",
  as_of = as_of_date) %>% 
  dplyr::select(target_end_date = date, hospitalizations = inc) %>%
  dplyr::filter(target_end_date >= analysis_start_date) 
ca_hosps <- covidData::load_data(
  measure = "hospitalizations",
  location_code = "06",
  temporal_resolution = "daily",
  source = "covidcast",
  as_of = as_of_date) %>% 
  dplyr::select(target_end_date = date, hospitalizations = inc) %>%
  dplyr::filter(target_end_date >= analysis_start_date) 

####figure for raw data####
ma_data <- ma_case_report %>%
  dplyr::full_join(ma_case_test, by = "target_end_date") %>%
  dplyr::full_join(ma_hosps,by = "target_end_date")
ma_case_report_hosp_ccf <- broom::tidy(ccf(ma_data$`report-date cases`,
                                           ma_data$hospitalizations,
                                           na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`report-date cases` = acf)
ma_case_test_hosp_ccf <- broom::tidy(ccf(ma_data$`test-date cases`, 
                                         ma_data$hospitalizations,
                                         na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`test-date cases` = acf)
ma_ccf <- dplyr::full_join(ma_case_report_hosp_ccf, ma_case_test_hosp_ccf, by = "lag") %>%
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(location = "Massachusetts")

ca_data <- ca_case_report %>%
  dplyr::full_join(ca_case_test, by = "target_end_date") %>%
  dplyr::full_join(ca_hosps,by = "target_end_date") %>%
  dplyr::full_join(ca_dph_case_report,by = "target_end_date") 
ca_case_report_hosp_ccf <- broom::tidy(ccf(ca_data$`report-date cases`, 
                                           ca_data$hospitalizations,
                                           na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`report-date cases` = acf)
ca_dph_case_report_hosp_ccf <- broom::tidy(ccf(ca_data$`CA DPH report-date cases`, 
                                           ca_data$hospitalizations,
                                           na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`CA DPH report-date cases` = acf)
ca_case_test_hosp_ccf <- broom::tidy(ccf(ca_data$`test-date cases`, 
                                         ca_data$hospitalizations,
                                         na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`test-date cases` = acf)
ca_ccf <- dplyr::full_join(ca_case_report_hosp_ccf, ca_case_test_hosp_ccf, by = "lag") %>%
  dplyr::full_join(ca_dph_case_report_hosp_ccf, by = "lag")%>%
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(location = "California")

vertical_line_layer_ma <- ma_ccf %>%
  dplyr::group_by(Data) %>%
  dplyr::summarize(xint = lag[value == max(value)],
                   max_ccf = max(value)) %>%
  dplyr::ungroup()%>%
  dplyr::select(-Data)

vertical_line_layer_ca <- ca_ccf %>%
  dplyr::group_by(Data) %>%
  dplyr::summarize(xint = lag[value == max(value)],
                   max_ccf = max(value)) %>%
  dplyr::ungroup()%>%
  dplyr::select(-Data)

p_ccf_ma <- ggplot(data = ma_ccf, 
                aes(x = lag, y = value, color = Data)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = c( "report-date cases" = "#F8766D", "test-date cases" = "#00BFC4","CA DPH report-date cases" = "#00BA38"))+
  geom_vline(aes(xintercept=0), linetype="solid", color = "darkgrey")+
  geom_segment(aes(x=xint, y = 0, xend= xint, yend = max_ccf), 
               data=vertical_line_layer_ma, linetype="dashed",
               color = "black")+
  ylab("")+
  xlab("")+
  ylim(0,1)+
  scale_x_continuous(breaks = c(-25, -20,-10, 0, 10, 20, 25,
                                vertical_line_layer_ma$xint),
                     minor_breaks = NULL)+
  theme(legend.position = "right",
        text = element_text(size = 12),
        plot.margin = unit(c(2, 5, 1, 5), "pt")) +
  facet_wrap(location ~., scales = "free_y", nrow = 1) 

p_ccf_ca <- ggplot(data = ca_ccf, 
                   aes(x = lag, y = value, color = Data)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = c( "report-date cases" = "#F8766D", "test-date cases" = "#00BFC4","CA DPH report-date cases" = "#00BA38"))+
  geom_vline(aes(xintercept=0), linetype="solid", color = "darkgrey")+
  geom_segment(aes(x=xint, y = 0, xend= xint, yend = max_ccf), 
               data=vertical_line_layer_ca, linetype="dashed",
               color = "black")+
  ylab("")+
  xlab("Lag")+
  ylim(0,1)+
  scale_x_continuous(breaks = c(-25, -20,-10, 0, 10, 20, 25,
                                vertical_line_layer_ca$xint),
                      minor_breaks = NULL) +
  theme(legend.position = "right",
        text = element_text(size = 12),
        plot.margin = unit(c(-6, 5, 1, 5), "pt")) +
  facet_wrap(location ~., scales = "free_y", nrow = 1) 

p2 <- ggpubr::ggarrange(p_ccf_ma, ggplot() + theme_void(), p_ccf_ca,
                        heights = c(1.2, 0.01,1.2),
                        ncol =1,common.legend = TRUE, legend="right",
                        align = "v")
annotate_figure(p2, left = textGrob("Cross-correlation Between Hospitalizations and Cases", rot = 90, vjust = 1, gp = gpar(fontsize = 12)))
ggsave('fig2_raw_CA_DPH.jpeg',dpi=300)

####figure for smoothed data####
ma_case_test<- ma_case_test %>%
  arrange(target_end_date) %>%
  dplyr::mutate(`smooth test-date cases` = slider::slide_dbl(`test-date cases`, 
                                                             mean,
                                                             .before = 6, 
                                                             .after=0, .complete=FALSE))
ma_case_report<- ma_case_report %>%
  arrange(target_end_date) %>%
  dplyr::mutate(`smooth report-date cases` = slider::slide_dbl(`report-date cases`, 
                                                               mean,
                                                               .before = 6, 
                                                               .after=0, .complete=FALSE))
ma_hosps<- ma_hosps %>%
  arrange(target_end_date) %>%
  dplyr::mutate(`smooth hospitalizations` = slider::slide_dbl(hospitalizations, mean,
                                                              .before = 6, .after=0, 
                                                              .complete=FALSE))
ca_case_test<- ca_case_test %>%
  arrange(target_end_date) %>%
  dplyr::mutate(`smooth test-date cases` = slider::slide_dbl(`test-date cases`, 
                                                             mean,
                                                             .before = 6, 
                                                             .after=0, .complete=FALSE))
ca_dph_case_report<- ca_dph_case_report %>%
  arrange(target_end_date) %>%
  dplyr::mutate(`smooth CA DPH report-date cases` = slider::slide_dbl(`CA DPH report-date cases`, 
                                                               mean,
                                                               .before = 6, 
                                                               .after=0, .complete=FALSE))

ca_case_report<- ca_case_report %>%
  arrange(target_end_date) %>%
  dplyr::mutate(`smooth report-date cases` = slider::slide_dbl(`report-date cases`, 
                                                               mean,
                                                               .before = 6, 
                                                               .after=0, .complete=FALSE))
ca_hosps<- ca_hosps %>%
  arrange(target_end_date) %>%
  dplyr::mutate(`smooth hospitalizations` = slider::slide_dbl(hospitalizations, mean,
                                                              .before = 6, .after=0, 
                                                              .complete=FALSE))

ma_data <- ma_case_report %>%
  dplyr::full_join(ma_case_test, by = "target_end_date") %>%
  dplyr::full_join(ma_hosps, by = "target_end_date") %>%
  dplyr::select(-hospitalizations,-`test-date cases`, -`report-date cases`)

ma_case_report_hosp_ccf <- broom::tidy(ccf(ma_data$`smooth report-date cases`,
                                           ma_data$`smooth hospitalizations`,
                                           na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth report-date cases` = acf)
ma_case_test_hosp_ccf <- broom::tidy(ccf(ma_data$`smooth test-date cases`, 
                                         ma_data$`smooth hospitalizations`,
                                         na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth test-date cases` = acf)
ma_ccf <- dplyr::full_join(ma_case_report_hosp_ccf, ma_case_test_hosp_ccf, by = "lag") %>%
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(location = "Massachusetts")
  
ca_data <- ca_case_report %>%
  dplyr::full_join(ca_case_test, by = "target_end_date") %>%
  dplyr::full_join(ca_hosps, by = "target_end_date") %>%
  dplyr::full_join(ca_dph_case_report, by = "target_end_date") %>%
  dplyr::select(-hospitalizations,-`test-date cases`, -`report-date cases`, -`CA DPH report-date cases`) 

ca_case_report_hosp_ccf <- broom::tidy(ccf(ca_data$`smooth report-date cases`, 
                                           ca_data$`smooth hospitalizations`,
                                           na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth report-date cases` = acf)
ca_dph_case_report_hosp_ccf <- broom::tidy(ccf(ca_data$`smooth CA DPH report-date cases`, 
                                           ca_data$`smooth hospitalizations`,
                                           na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth CA DPH report-date cases` = acf)
ca_case_test_hosp_ccf <- broom::tidy(ccf(ca_data$`smooth test-date cases`, 
                                         ca_data$`smooth hospitalizations`,
                                         na.action=na.omit, plot = FALSE)) %>%
  dplyr::rename(`smooth test-date cases` = acf)
ca_ccf <- dplyr::full_join(ca_case_report_hosp_ccf, ca_case_test_hosp_ccf, by = "lag") %>%
  dplyr::full_join(ca_dph_case_report_hosp_ccf, by = "lag") %>%
  tidyr::pivot_longer(!lag, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(location = "California")

vertical_line_layer_ma <- ma_ccf %>%
  dplyr::group_by(Data) %>%
  dplyr::summarize(xint = lag[value == max(value)],
                   max_ccf = max(value)) %>%
  dplyr::ungroup()%>%
  dplyr::select(-Data)

vertical_line_layer_ca <- ca_ccf %>%
  dplyr::group_by(Data) %>%
  dplyr::summarize(xint = lag[value == max(value)],
                   max_ccf = max(value)) %>%
  dplyr::ungroup()%>%
  dplyr::select(-Data)

p_ccf_ma <- ggplot(data = ma_ccf, 
                   aes(x = lag, y = value, color = Data)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = c( "smooth report-date cases" = "#F8766D", "smooth test-date cases" = "#00BFC4","smooth CA DPH report-date cases" = "#00BA38"))+
  geom_vline(aes(xintercept=0), linetype="solid", color = "darkgrey")+
  geom_segment(aes(x=xint, y = 0, xend= xint, yend = max_ccf), 
               data=vertical_line_layer_ma, linetype="dashed",
               color = "black")+
  ylab("")+
  xlab("")+
  ylim(0,1)+
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        plot.margin = unit(c(2, 5, 1, 5), "pt")) +
  scale_x_continuous(breaks = c(-25, -20,-10, 0, 10, 20, 25,
                                vertical_line_layer_ma$xint),
                     minor_breaks = NULL)+
  facet_wrap(location ~., scales = "free_y", nrow = 1) 

p_ccf_ca <- ggplot(data = ca_ccf, 
                   aes(x = lag, y = value, color = Data)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = c( "smooth report-date cases" = "#F8766D", "smooth test-date cases" = "#00BFC4","smooth CA DPH report-date cases" = "#00BA38"))+
  geom_vline(aes(xintercept=0), linetype="solid", color = "darkgrey")+
  geom_segment(aes(x=xint, y = 0, xend= xint, yend = max_ccf), 
               data=vertical_line_layer_ca, linetype="dashed",
               color = "black")+
  ylab("")+
  xlab("Lag")+
  ylim(0,1)+
  theme(legend.position = "none",
        text = element_text(size = 12),
        plot.margin = unit(c(-6, 5, 1, 5), "pt")) +
  scale_x_continuous(breaks = c(-25, -20,-10, 0, 10, 20, 25,
                                vertical_line_layer_ca$xint),
                     minor_breaks = NULL) +
  facet_wrap(location ~., scales = "free_y", nrow = 1) 

p2 <- ggpubr::ggarrange(p_ccf_ma, ggplot() + theme_void(), p_ccf_ca,
                        heights = c(1.2, 0.01,1.2),
                        ncol =1,common.legend = TRUE, legend="right",
                        align = "v")
annotate_figure(p2, left = textGrob("Cross-correlation Between Hospitalizations and Cases", rot = 90, vjust = 1, gp = gpar(fontsize = 12)))
ggsave('fig2_smooth_CA_DPH.jpeg',dpi=300)

