# figure 1
library(dplyr)
library(covidHubUtils)
library(ggplot2)
library(ggpubr)
library(forecast)
library(broom)
library(cowplot)

# use all data as of 2022-04-29
as_of_date <- as.Date("2022-04-29")
analysis_start_date <- as.Date("2020-10-01")
hosp_start_date <- as.Date("2020-08-01")
case_start_date <- as.Date("2020-03-01")
case_end_date <- as.Date("2022-04-28")
validation_date <- as.Date("2020-12-07")
validation_test_split <- as.Date("2021-06-07")

ma_case_test <- readr::read_csv("csv-data/MA-DPH-csvdata-covid-2022-04-28.csv") %>%
  dplyr::select(target_end_date = test_date, `test-date cases` = new_positive) %>%
  dplyr::filter(target_end_date >= analysis_start_date, target_end_date <= case_end_date) 
ca_case_test <- readr::read_csv("csv-data/CA-DPH-testdate-covid-2022-04-29.csv") %>%
  dplyr::select(target_end_date = test_date, `test-date cases` = new_positive) %>%
  dplyr::filter(target_end_date >= analysis_start_date, target_end_date <= case_end_date)
ca_dph_case_report <- readr::read_csv("csv-data/CA-DPH-reportdate-covid-2022-04-29.csv") %>%
  dplyr::select(target_end_date = report_date, `CA DPH report-date cases` = new_positive) %>%
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
ma_hosps <-  covidData::load_data(
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
  dplyr::full_join(ma_hosps, by = "target_end_date") %>%
  dplyr::mutate(hospitalizations = hospitalizations *  15) %>%
  tidyr::pivot_longer(!target_end_date, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(group = ifelse(Data == "hospitalizations", 
                               "Hospitalizations", 
                               "Cases")) %>%
  dplyr::mutate(facet_title = "Massachusetts Observations")

ca_data <- ca_case_report %>%
  dplyr::full_join(ca_case_test, by = "target_end_date") %>%
  dplyr::full_join(ca_hosps, by = "target_end_date") %>%
  dplyr::full_join(ca_dph_case_report, by = "target_end_date") %>%
  dplyr::mutate(hospitalizations = hospitalizations * 15) %>%
  tidyr::pivot_longer(!target_end_date, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(group = ifelse(Data == "hospitalizations", 
                               "Hospitalizations", 
                               "Cases")) %>%
  dplyr::mutate(facet_title = "California Observations")

p1_1_ma <- ggplot2::ggplot(data = ma_data, 
                        aes(x = target_end_date, y = value, color = Data, group = desc(Data))) +
  geom_line() + 
  geom_vline(xintercept = as.numeric(validation_test_split),
             linetype="dashed", colour="black") + 
  geom_vline(xintercept = as.numeric(validation_date),
             linetype="dashed", colour="black") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  theme_bw() +
  facet_wrap(facet_title ~., scales = "free_y", nrow = 1) +
  annotate("text", x=validation_date, y=65000, label= "Validation Period", size = 3,hjust = -0.1) + 
  annotate("text", x=validation_test_split, y=65000, label= "Test Period", size = 3,hjust = -0.1) + 
  scale_color_manual(values = c("hospitalizations"= "#00AFBB", "report-date cases" = "#E7B800", "test-date cases" = "#FC4E07","CA DPH report-date cases" = "#778A35")) +
  scale_y_continuous(name = "Reported Cases", 
                     # this is decided manually 
                     sec.axis = sec_axis(~(.*1/15),
                                         name = "Reported Hospitalizations"))+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %Y", 
               limits = c(analysis_start_date, as_of_date),
               expand=c(0.02,0.02)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.title.x  = element_blank(),
        legend.position = "none",
        text = element_text(size = 10),
        plot.margin = unit(c(2, 5, -1, 5), "pt"))

p1_3_ca <- ggplot2::ggplot(data = ca_data, 
                           aes(x = target_end_date, y = value, color = Data, group = desc(Data))) +
  geom_line() + 
  geom_vline(xintercept = as.numeric(validation_test_split),
             linetype="dashed", colour="black") + 
  geom_vline(xintercept = as.numeric(validation_date),
             linetype="dashed", colour="black") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  theme_bw() +
  facet_wrap(facet_title ~., scales = "free_y", nrow = 1) +
  annotate("text", x=validation_date, y=300000, label= "Validation Period", size = 3,hjust = -0.1) + 
  annotate("text", x=validation_test_split, y=300000, label= "Test Period", size = 3,hjust = -0.1) +
  scale_color_manual(values = c("hospitalizations"= "#00AFBB", "report-date cases" = "#E7B800", "test-date cases" = "#FC4E07", "CA DPH report-date cases" = "#778A35")) +
  scale_y_continuous(name = "Reported Cases", 
                     sec.axis = sec_axis(~(.*1/15),
                                         name = "Reported Hospitalizations"))+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %Y", 
               limits = c(analysis_start_date, as_of_date),
               expand=c(0.02,0.02)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.title.x  = element_blank(),
        plot.title = element_text(vjust = - 10),
        legend.position = "none",
        plot.margin = unit(c(0, 5, -1, 5), "pt"),
        text = element_text(size =10))
# weekly relative change
ma_data_rel_inc_7d <- ma_data %>%
  dplyr::group_by(facet_title, Data) %>%
  dplyr::arrange(target_end_date) %>%
  dplyr::mutate(rel_inc_7d = (value)/lag(value,7)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(facet_title = "Massachusetts Growth Rates")

ca_data_rel_inc_7d <- ca_data %>%
  dplyr::group_by(facet_title, Data) %>%
  dplyr::arrange(target_end_date) %>%
  dplyr::mutate(rel_inc_7d = (value)/lag(value,7)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(facet_title = "California Growth Rates")

# rel_inc figure
p_growth_ma <- ggplot(data = ma_data_rel_inc_7d, 
                   aes(x = target_end_date, y = rel_inc_7d, color = Data)) +
  geom_smooth(span=.15, se=FALSE, lwd = 0.5) +
  geom_point(alpha=.1) +
  geom_hline(yintercept=1, linetype=2, alpha=.7) +
  geom_vline(xintercept = as.numeric(validation_test_split),
             linetype="dashed", colour="black")+
  geom_vline(xintercept = as.numeric(validation_date),
             linetype="dashed", colour="black") + 
  annotate("text", x=validation_date, y=2.3, label= "Validation Period", size = 3,hjust = -0.1) + 
  annotate("text", x=validation_test_split, y=2.3, label= "Test Period", size = 3,hjust = -0.1) + 
  scale_y_log10(name="Relative 1 Week Change\n (Log Scale)",
                sec.axis = sec_axis(~.,
                                    breaks = NULL,
                                    name = "")
                )+
  theme_bw() +
  scale_color_manual(values = c("hospitalizations"= "#00AFBB", "report-date cases" = "#E7B800", "test-date cases" = "#FC4E07","CA DPH report-date cases" = "#778A35")) +
  facet_wrap(facet_title ~., scales = "free_y", nrow = 1) +
  scale_x_date(name=NULL, 
               date_breaks = "1 month", 
               date_labels = "%b %Y", 
               limits = c(analysis_start_date, as_of_date),
               expand=c(0.02,0.02)) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title = element_text(vjust = - 10),
        text = element_text(size = 10),
        plot.margin = unit(c(0, 5, -1, 5), "pt")) +
  coord_cartesian(ylim=c(0.5, 2.5))

p_growth_ca <- ggplot(data = ca_data_rel_inc_7d, 
                      aes(x = target_end_date, y = rel_inc_7d, color = Data)) +
  geom_smooth(span=.15, se=FALSE, lwd = 0.5) +
  geom_point(alpha=.1) +
  geom_hline(yintercept=1, linetype=2, alpha=.7) +
  geom_vline(xintercept = as.numeric(validation_test_split),
             linetype="dashed", colour="black")+
  geom_vline(xintercept = as.numeric(validation_date),
             linetype="dashed", colour="black") + 
  annotate("text", x=validation_date, y=2.3, label= "Validation Period", size = 3,hjust = -0.1) + 
  annotate("text", x=validation_test_split, y=2.3, label= "Test Period", size = 3,hjust = -0.1) + 
  scale_y_log10(name="Relative 1 Week Change\n (Log Scale)",
                sec.axis = sec_axis(~.,
                                    breaks = NULL,
                                    name = ""))+
  theme_bw() +
  scale_color_manual(values = c("hospitalizations"= "#00AFBB", "report-date cases" = "#E7B800", "test-date cases" = "#FC4E07","CA DPH report-date cases" = "#778A35")) +
  facet_wrap(facet_title ~., scales = "free_y", nrow = 1) +
  scale_x_date(name=NULL, 
               date_breaks = "1 month", 
               date_labels = "%b %Y", 
               limits = c(analysis_start_date, as_of_date),
               expand=c(0.02,0.02)) +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=90, hjust=0.5, vjust = 2.5),
        plot.title = element_text(vjust = - 10),
        text = element_text(size = 10),
        plot.margin = unit(c(0, 5, -1, 5), "pt")) +
  coord_cartesian(ylim=c(0.5, 2.5))

p1 <- ggpubr::ggarrange(p1_1_ma, ggplot() + theme_void(),p_growth_ma, ggplot() + theme_void(),p1_3_ca,ggplot() + theme_void(),p_growth_ca,
                        heights = c(1, 0.02,1, 0.02,1, 0.02,1.3),
                        labels = c("A","", "B","", "C", "","D"),
                        ncol =1,common.legend = TRUE, legend="bottom",
                        align = "hv")
ggsave('fig1_raw_CA_DPH.jpeg',dpi=300)

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
ma_raw_bubble <- ma_case_report %>%
  dplyr::full_join(ma_case_test, by = "target_end_date") %>%
  dplyr::full_join(ma_hosps, by = "target_end_date") %>%
  dplyr::mutate(hospitalizations = hospitalizations *  15) %>%
  dplyr::select(-`smooth hospitalizations`,-`smooth test-date cases`, -`smooth report-date cases`) %>%
  dplyr::rename(`smooth hospitalizations`= hospitalizations,
                `smooth test-date cases`=`test-date cases`, 
                `smooth report-date cases`=`report-date cases`) %>%
  tidyr::pivot_longer(!target_end_date, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(group = ifelse(Data == "smooth hospitalizations", 
                               "Hospitalizations", 
                               "Cases")) %>%
  dplyr::mutate(facet_title = "Massachusetts Observations")

ma_data <- ma_case_report %>%
  dplyr::full_join(ma_case_test, by = "target_end_date") %>%
  dplyr::full_join(ma_hosps, by = "target_end_date") %>%
  dplyr::mutate(`smooth hospitalizations` = `smooth hospitalizations` *  15) %>%
  dplyr::select(-hospitalizations,-`test-date cases`, -`report-date cases`) %>%
  tidyr::pivot_longer(!target_end_date, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(group = ifelse(Data == "smooth hospitalizations", 
                               "Hospitalizations", 
                               "Cases")) %>%
  dplyr::mutate(facet_title = "Massachusetts Observations")

ca_raw_bubble <- ca_case_report %>%
  dplyr::full_join(ca_case_test, by = "target_end_date") %>%
  dplyr::full_join(ca_hosps, by = "target_end_date") %>%
  dplyr::full_join(ca_dph_case_report, by = "target_end_date") %>%
  dplyr::mutate(hospitalizations = hospitalizations *  15) %>%
  dplyr::select(-`smooth hospitalizations`,-`smooth test-date cases`, -`smooth report-date cases`, -`smooth CA DPH report-date cases`) %>%
  dplyr::rename(`smooth hospitalizations`=hospitalizations,
                `smooth test-date cases`=`test-date cases`, 
                `smooth report-date cases`=`report-date cases`,
                `smooth CA DPH report-date cases`= `CA DPH report-date cases`) %>%
  tidyr::pivot_longer(!target_end_date, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(group = ifelse(Data == "smooth hospitalizations", 
                               "Hospitalizations", 
                               "Cases")) %>%
  dplyr::mutate(facet_title = "California Observations")

ca_data <- ca_case_report %>%
  dplyr::full_join(ca_case_test, by = "target_end_date") %>%
  dplyr::full_join(ca_hosps, by = "target_end_date") %>%
  dplyr::full_join(ca_dph_case_report, by = "target_end_date") %>%
  dplyr::mutate(`smooth hospitalizations` = `smooth hospitalizations` *  15) %>%
  dplyr::select(-hospitalizations,-`test-date cases`, -`report-date cases`, -`CA DPH report-date cases`) %>%
  tidyr::pivot_longer(!target_end_date, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(group = ifelse(Data == "smooth hospitalizations", 
                               "Hospitalizations", 
                               "Cases")) %>%
  dplyr::mutate(facet_title = "California Observations")
p1_1_ma <- ggplot2::ggplot(data = ma_data, 
                           aes(x = target_end_date, y = value, color = Data, group = desc(Data))) +
  geom_line() + 
  geom_point(data = ma_raw_bubble, aes(x = target_end_date, y = value, color = Data, group = desc(Data)), alpha = 0.3) + 
  geom_vline(xintercept = as.numeric(validation_test_split),
             linetype="dashed", colour="black") + 
  geom_vline(xintercept = as.numeric(validation_date),
             linetype="dashed", colour="black") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  theme_bw() +
  facet_wrap(facet_title ~., scales = "free_y", nrow = 1) +
  annotate("text", x=validation_date, y=65000, label= "Validation Period", size = 3,hjust = -0.1) + 
  annotate("text", x=validation_test_split, y=65000, label= "Test Period", size = 3,hjust = -0.1) + 
  #labs(x= "") +
  scale_color_manual(values = c("smooth hospitalizations"= "#00AFBB", 
                                "smooth report-date cases" = "#E7B800", 
                                "smooth test-date cases" = "#FC4E07",
                                "smooth CA DPH report-date cases" = "#778A35")) +
  scale_y_continuous(name = "Reported Cases", 
                     sec.axis = sec_axis(~(.*1/15),
                                         name = "Reported Hospitalizations"))+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %Y", 
               limits = c(analysis_start_date, as_of_date),
               expand=c(0.02,0.02)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.title.x  = element_blank(),
        legend.position = "none",
        text = element_text(size = 10),
        plot.margin = unit(c(2, 5, -1, 5), "pt"))


p1_3_ca <- ggplot2::ggplot(data = ca_data, 
                           aes(x = target_end_date, y = value, color = Data, group = desc(Data))) +
  geom_line() + 
  geom_point(data = ca_raw_bubble, aes(x = target_end_date, y = value, color = Data, group = desc(Data)), alpha = 0.3) + 
  geom_vline(xintercept = as.numeric(validation_test_split),
             linetype="dashed", colour="black") + 
  geom_vline(xintercept = as.numeric(validation_date),
             linetype="dashed", colour="black") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  theme_bw() +
  facet_wrap(facet_title ~., scales = "free_y", nrow = 1) +
  annotate("text", x=validation_date, y=300000, label= "Validation Period", size = 3,hjust = -0.1) + 
  annotate("text", x=validation_test_split, y=300000, label= "Test Period", size = 3,hjust = -0.1) + 
  #labs(x= "") +
  scale_color_manual(values = c("smooth hospitalizations"= "#00AFBB", 
                                "smooth report-date cases" = "#E7B800", 
                                "smooth test-date cases" = "#FC4E07",
                                "smooth CA DPH report-date cases" = "#778A35")) +
  scale_y_continuous(name = "Reported Cases", 
                     sec.axis = sec_axis(~(.*1/15),
                                         name = "Reported Hospitalizations"))+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %Y", 
               limits = c(analysis_start_date, as_of_date),
               expand=c(0.02,0.02)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.title.x  = element_blank(),
        plot.title = element_text(vjust = - 10),
        legend.position = "none",
        plot.margin = unit(c(0, 5, -1, 5), "pt"),
        text = element_text(size =10))
# weekly relative change
ma_data_rel_inc_7d <- ma_data %>%
  dplyr::group_by(facet_title, Data) %>%
  dplyr::arrange(target_end_date) %>%
  dplyr::mutate(rel_inc_7d = (value)/lag(value,7)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(facet_title = "Massachusetts Growth Rates")

ca_data_rel_inc_7d <- ca_data %>%
  dplyr::group_by(facet_title, Data) %>%
  dplyr::arrange(target_end_date) %>%
  dplyr::mutate(rel_inc_7d = (value)/lag(value,7)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(facet_title = "California Growth Rates")

# rel_inc figure
p_growth_ma <- ggplot(data = ma_data_rel_inc_7d, 
                      aes(x = target_end_date, y = rel_inc_7d, color = Data)) +
  geom_smooth(span=.15, se=FALSE, lwd = 0.5) +
  geom_point(alpha=.1) +
  geom_hline(yintercept=1, linetype=2, alpha=.7) +
  geom_vline(xintercept = as.numeric(validation_test_split),
             linetype="dashed", colour="black")+
  geom_vline(xintercept = as.numeric(validation_date),
             linetype="dashed", colour="black") + 
  annotate("text", x=validation_date, y=2.3, label= "Validation Period", size = 3,hjust = -0.1) + 
  annotate("text", x=validation_test_split, y=2.3, label= "Test Period", size = 3,hjust = -0.1) + 
  scale_y_log10(name="Relative 1 Week Change\n (Log Scale)",
                sec.axis = sec_axis(~.,
                                    breaks = NULL,
                                    name = "")
  )+
  theme_bw() +
  scale_color_manual(values = c("smooth hospitalizations"= "#00AFBB", 
                                "smooth report-date cases" = "#E7B800", 
                                "smooth test-date cases" = "#FC4E07",
                                "smooth CA DPH report-date cases" = "#778A35")) +
  facet_wrap(facet_title ~., scales = "free_y", nrow = 1) +
  scale_x_date(name=NULL, 
               date_breaks = "1 month", 
               date_labels = "%b %Y", 
               limits = c(analysis_start_date, as_of_date),
               expand=c(0.02,0.02)) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title = element_text(vjust = - 10),
        text = element_text(size = 10),
        plot.margin = unit(c(0, 5, -1, 5), "pt")) +
  coord_cartesian(ylim=c(0.5, 2.5))

p_growth_ca <- ggplot(data = ca_data_rel_inc_7d, 
                      aes(x = target_end_date, y = rel_inc_7d, color = Data)) +
  geom_smooth(span=.15, se=FALSE, lwd = 0.5) +
  geom_point(alpha=.1) +
  geom_hline(yintercept=1, linetype=2, alpha=.7) +
  geom_vline(xintercept = as.numeric(validation_test_split),
             linetype="dashed", colour="black")+
  geom_vline(xintercept = as.numeric(validation_date),
             linetype="dashed", colour="black") + 
  annotate("text", x=validation_date, y=2.3, label= "Validation Period", size = 3,hjust = -0.1) + 
  annotate("text", x=validation_test_split, y=2.3, label= "Test Period", size = 3,hjust = -0.1) + 
  scale_y_log10(name="Relative 1 Week Change\n (Log Scale)",
                sec.axis = sec_axis(~.,
                                    breaks = NULL,
                                    name = ""))+
  theme_bw() +
  scale_color_manual(values = c("smooth hospitalizations"= "#00AFBB", 
                                "smooth report-date cases" = "#E7B800", 
                                "smooth test-date cases" = "#FC4E07",
                                "smooth CA DPH report-date cases" = "#778A35")) +
  facet_wrap(facet_title ~., scales = "free_y", nrow = 1) +
  scale_x_date(name=NULL, 
               date_breaks = "1 month", 
               date_labels = "%b %Y", 
               limits = c(analysis_start_date, as_of_date),
               expand=c(0.02,0.02)) +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=90, hjust=0.5, vjust = 2.5),
        plot.title = element_text(vjust = - 10),
        text = element_text(size = 10),
        plot.margin = unit(c(0, 5, -1, 5), "pt")) +
  coord_cartesian(ylim=c(0.5, 2.5))

p1 <- ggpubr::ggarrange(p1_1_ma, ggplot() + theme_void(),p_growth_ma, ggplot() + theme_void(),p1_3_ca,ggplot() + theme_void(),p_growth_ca,
                        labels = c("A","", "B","", "C", "","D"),
                        heights = c(1, 0.02,1, 0.02,1, 0.02,1.3),
                        ncol =1,common.legend = TRUE, legend="bottom",
                        align = "hv")
ggsave('fig1_smooth_CA_DPH.jpeg',dpi=300)
