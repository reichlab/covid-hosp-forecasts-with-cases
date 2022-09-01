# figure 1
library(dplyr)
library(ggplot2)
library(ggpubr)

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

####figure for raw data####
ma_data <- ma_case_report %>%
  dplyr::left_join(ma_case_test, by = "target_end_date") %>%
  dplyr::mutate(hospitalizations = hospitalizations *  15) %>%
  tidyr::pivot_longer(!target_end_date, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(group = ifelse(Data == "hospitalizations", 
                               "Hospitalizations", 
                               "Cases")) %>%
  dplyr::mutate(facet_title = "Massachusetts Observations")

ca_data <- ca_case_report_jhu %>%
  dplyr::left_join(ca_case_test, by = "target_end_date") %>%
  dplyr::left_join(ca_case_report_dph, by = "target_end_date") %>%
  dplyr::mutate(hospitalizations = hospitalizations * 15) %>%
  tidyr::pivot_longer(!target_end_date, names_to = "Data", values_to ="value") %>%
  dplyr::mutate(group = ifelse(Data == "hospitalizations", 
                               "Hospitalizations", 
                               "Cases")) %>%
  dplyr::mutate(facet_title = "California Observations")

p1_1_ma <- ggplot2::ggplot(mapping=aes(x = target_end_date, y = value, color = Data, group = desc(Data))) +
  geom_line(data = dplyr::filter(ma_data, Data %in% c("smooth JHU report-date cases", "smooth DPH test-date cases", "hospitalizations"))) + 
  geom_point(data = dplyr::filter(ma_data, Data %in% c("report-date cases", "test-date cases")), alpha=.2) + 
  geom_vline(xintercept = as.numeric(validation_test_split),
             linetype="dashed", colour="black") + 
  geom_vline(xintercept = as.numeric(validation_date),
             linetype="dashed", colour="black") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  theme_bw() +
  facet_wrap(facet_title ~., scales = "free_y", nrow = 1) +
  annotate("text", x=validation_date, y=65000, label= "Validation Period", size = 3,hjust = -0.1) + 
  annotate("text", x=validation_test_split, y=65000, label= "Test Period", size = 3,hjust = -0.1) + 
  scale_color_manual(values = c("hospitalizations"= "#00AFBB", 
                                "report-date cases" = "#E7B800", 
                                "smooth JHU report-date cases" = "#E7B800", 
                                "smooth DPH test-date cases" = "#FC4E07",
                                "test-date cases" = "#FC4E07")) +
  scale_y_log10(name = "Reported Cases\n (log scale)", labels = scales::comma,
                # this is decided manually 
                sec.axis = sec_axis(~(.*1/15), labels = scales::comma,
                                    name = "Reported Hospitalizations\n (log scale)"))+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %Y", 
               limits = c(analysis_start_date, as_of_date),
               expand=c(0.02,0.02)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.title.x  = element_blank(),
        axis.ticks.y.right = element_line(color = "#00AFBB"),
        axis.line.y.right = element_line(color = "#00AFBB"),
        axis.text.y.right = element_text(color = "#00AFBB"),
        legend.position = "none",
        text = element_text(size = 10),
        plot.margin = unit(c(2, 5, -1, 5), "pt"))

p1_3_ca <- ggplot2::ggplot(mapping=aes(x = target_end_date, y = value, color = Data, group = desc(Data))) +
  geom_line(data = dplyr::filter(ca_data, Data %in% c("smooth JHU report-date cases", "smooth DPH test-date cases", "smooth CA DPH report-date cases", "hospitalizations"))) + 
  geom_point(data = dplyr::filter(ca_data, Data %in% c("report-date cases", "test-date cases", "CA DPH report-date cases")), alpha=.2) +  
  geom_vline(xintercept = as.numeric(validation_test_split),
             linetype="dashed", colour="black") + 
  geom_vline(xintercept = as.numeric(validation_date),
             linetype="dashed", colour="black") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  theme_bw() +
  facet_wrap(facet_title ~., scales = "free_y", nrow = 1) +
  annotate("text", x=validation_date, y=300000, label= "Validation Period", size = 3,hjust = -0.1) + 
  annotate("text", x=validation_test_split, y=300000, label= "Test Period", size = 3,hjust = -0.1) +
  scale_color_manual(values = c("hospitalizations"= "#00AFBB", 
                                "report-date cases" = "#E7B800", 
                                "test-date cases" = "#FC4E07", 
                                "smooth JHU report-date cases" = "#E7B800", 
                                "smooth DPH test-date cases" = "#FC4E07",
                                "smooth CA DPH report-date cases" = "#778A35",
                                "CA DPH report-date cases" = "#778A35")) +
  scale_y_log10(name = "Reported Cases\n (log scale)", 
                labels = scales::comma,
                sec.axis = sec_axis(~(.*1/15), labels = scales::comma,
                                    name = "Reported Hospitalizations\n (log scale)"))+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %Y", 
               limits = c(analysis_start_date, as_of_date),
               expand=c(0.02,0.02)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.title.x  = element_blank(),
        axis.ticks.y.right = element_line(color = "#00AFBB"),
        axis.line.y.right = element_line(color = "#00AFBB"),
        axis.text.y.right = element_text(color = "#00AFBB"),
        plot.title = element_text(vjust = - 10),
        legend.position = "none",
        plot.margin = unit(c(0, 5, -1, 5), "pt"),
        text = element_text(size =10))

# weekly relative change
ma_data_rel_inc_7d <- ma_data %>%
  dplyr::filter(Data %in% c("hospitalizations", "smooth DPH test-date cases", "smooth JHU report-date cases")) %>%
  dplyr::group_by(facet_title, Data) %>%
  dplyr::arrange(target_end_date) %>%
  dplyr::mutate(rel_inc_7d = (value)/lag(value,7),
                smooth_rel_inc_7d = slider::slide_dbl(rel_inc_7d, mean,
                                                      .before = 10, .after=10, 
                                                      .complete=FALSE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(facet_title = "Massachusetts Growth Rates")

ca_data_rel_inc_7d <- ca_data %>%
  dplyr::filter(Data %in% c("hospitalizations", "smooth DPH test-date cases", "smooth JHU report-date cases", "smooth CA DPH report-date cases")) %>%
  dplyr::group_by(facet_title, Data) %>%
  dplyr::arrange(target_end_date) %>%
  dplyr::mutate(rel_inc_7d = (value)/lag(value,7),
                smooth_rel_inc_7d = slider::slide_dbl(rel_inc_7d, mean,
                                                      .before = 10, .after=10, 
                                                      .complete=FALSE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(facet_title = "California Growth Rates")

# rel_inc figure
p_growth_ma <- ggplot(data = ma_data_rel_inc_7d, 
                   aes(x = target_end_date, color = Data)) +
  geom_line(aes(y=smooth_rel_inc_7d)) +
  geom_point(aes(y = rel_inc_7d), alpha=.1) +
  geom_hline(yintercept=1, linetype=2, alpha=.7) +
  geom_vline(xintercept = as.numeric(validation_test_split),
             linetype="dashed", colour="black")+
  geom_vline(xintercept = as.numeric(validation_date),
             linetype="dashed", colour="black") + 
  annotate("text", x=validation_date, y=max(ma_data_rel_inc_7d$rel_inc_7d, na.rm=TRUE), label= "Validation Period", size = 3,hjust = -0.1, vjust=1) + 
  annotate("text", x=validation_test_split, y=max(ma_data_rel_inc_7d$rel_inc_7d, na.rm=TRUE), label= "Test Period", size = 3,hjust = -0.1, vjust=1) + 
  scale_y_log10(name="Relative 1 Week Change\n (log scale)",
                sec.axis = sec_axis(~.,
                                    breaks = NULL,
                                    name = "")
                )+
  theme_bw() +
  scale_color_manual(values = c("hospitalizations"= "#00AFBB", 
                                "smooth JHU report-date cases" = "#E7B800", 
                                "smooth DPH test-date cases" = "#FC4E07")) +
  facet_wrap(facet_title ~., scales = "free_y", nrow = 1) +
  scale_x_date(name=NULL, 
               date_breaks = "1 month", 
               date_labels = "%b %Y", 
               limits = c(analysis_start_date, as_of_date),
               expand=c(0.02,0.02)) +
  #coord_cartesian(ylim=c(0.5, 2.5)) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title = element_text(vjust = - 10),
        text = element_text(size = 10),
        plot.margin = unit(c(0, 5, -1, 5), "pt"))

p_growth_ca <- ggplot(data = ca_data_rel_inc_7d, 
                      aes(x = target_end_date, y = rel_inc_7d, color = Data)) +
  geom_line(aes(y=smooth_rel_inc_7d)) +
  geom_point(alpha=.1) +
  geom_hline(yintercept=1, linetype=2, alpha=.7) +
  geom_vline(xintercept = as.numeric(validation_test_split),
             linetype="dashed", colour="black")+
  geom_vline(xintercept = as.numeric(validation_date),
             linetype="dashed", colour="black") + 
  annotate("text", x=validation_date, y=max(ca_data_rel_inc_7d$rel_inc_7d, na.rm=TRUE), label= "Validation Period", size = 3,hjust = -0.1, vjust=1) + 
  annotate("text", x=validation_test_split, y=max(ca_data_rel_inc_7d$rel_inc_7d, na.rm=TRUE), label= "Test Period", size = 3,hjust = -0.1, vjust=1) + 
  scale_y_log10(name="Relative 1 Week Change\n (log scale)",
                sec.axis = sec_axis(~.,
                                    breaks = NULL,
                                    name = ""))+
  theme_bw() +
  scale_color_manual(values = c("hospitalizations"= "#00AFBB", 
                                "smooth JHU report-date cases" = "#E7B800", 
                                "smooth DPH test-date cases" = "#FC4E07",
                                "smooth CA DPH report-date cases" = "#778A35")) +
  facet_wrap(facet_title ~., scales = "free_y", nrow = 1) +
  scale_x_date(name=NULL, 
               date_breaks = "1 month", 
               date_labels = "%b %Y", 
               limits = c(analysis_start_date, as_of_date),
               expand=c(0.02,0.02)) +
  #coord_cartesian(ylim=c(0.5, 2.5)) +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=90, hjust=0.5, vjust = 2.5),
        plot.title = element_text(vjust = - 10),
        text = element_text(size = 10),
        plot.margin = unit(c(0, 5, -1, 5), "pt")) 

p1 <- ggpubr::ggarrange(p1_1_ma, ggplot() + theme_void(),
                        p_growth_ma, ggplot() + theme_void(),
                        p1_3_ca,ggplot() + theme_void(),
                        p_growth_ca,
                        heights = c(1, 0.02,1, 0.02,1, 0.02,1.7),
                        labels = c("A","", "B","", "C", "","D"),
                        ncol =1,
                        common.legend = FALSE, 
                        #legend="none",
                        align = "hv")

ggsave('figures/fig1_smooth.jpeg', plot=p1, dpi=300, width = 8.5, height=11)
