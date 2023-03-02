source("code/all-forecast-plots.R")

plot_case_hosp_forecasts(forecast_date = "2021-07-12", state="ca")

ca1 <- plot_case_hosp_forecasts(forecast_date = "2021-07-12", state="ca", include_ca_dph_reportcase = FALSE, window_around_forecast_date = 90)
ca2 <- plot_case_hosp_forecasts(forecast_date = "2021-07-19", state="ca", include_ca_dph_reportcase = FALSE, window_around_forecast_date = 90)
ca3 <- plot_case_hosp_forecasts(forecast_date = "2021-07-26", state="ca", include_ca_dph_reportcase = FALSE, window_around_forecast_date = 90)

ma1 <- plot_case_hosp_forecasts(forecast_date = "2021-07-12", state="ma", include_ca_dph_reportcase = FALSE, window_around_forecast_date = 90)
ma2 <- plot_case_hosp_forecasts(forecast_date = "2021-07-19", state="ma", include_ca_dph_reportcase = FALSE, window_around_forecast_date = 90)
ma3 <- plot_case_hosp_forecasts(forecast_date = "2021-07-26", state="ma", include_ca_dph_reportcase = FALSE, window_around_forecast_date = 90)
ma4 <- plot_case_hosp_forecasts(forecast_date = "2022-05-30", state="ma", include_ca_dph_reportcase = FALSE, window_around_forecast_date = 90)

pdf(file = "plots/ca-202107.pdf", height=15, width=8)
cowplot::plot_grid(ca1, ca2, ca3, nrow=3)
cowplot::plot_grid(ma1, ma2, ma3, nrow=3)
dev.off()


forecast_dates <- as.character(seq.Date(as.Date("2020-12-07"), as.Date("2022-05-30"), by="7 days")) # c("2021-07-12", "2021-07-19", "2021-07-26")
states <- c("ca", "ma")
pdf(file = "plots/all-dates-data-and-forecasts.pdf", height=7, width=11)
for (state in states){
  for(forecast_date in forecast_dates){
    print(plot_case_hosp_forecasts(forecast_date = forecast_date, state=state, include_ca_dph_reportcase = FALSE, window_around_forecast_date = 90))
  }
}
dev.off()