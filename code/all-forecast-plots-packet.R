source("code/all-forecast-plots.R")

ca1 <- plot_case_hosp_forecasts(forecast_date = "2021-07-12", state="ca")
ca2 <- plot_case_hosp_forecasts(forecast_date = "2021-07-19", state="ca")
ca3 <- plot_case_hosp_forecasts(forecast_date = "2021-07-26", state="ca")

ma1 <- plot_case_hosp_forecasts(forecast_date = "2021-07-12", state="ma")
ma2 <- plot_case_hosp_forecasts(forecast_date = "2021-07-19", state="ma")
ma3 <- plot_case_hosp_forecasts(forecast_date = "2021-07-26", state="ma")
ma4 <- plot_case_hosp_forecasts(forecast_date = "2022-05-30", state="ma")

pdf(file = "plots/ca-202107.pdf", height=15, width=8)
cowplot::plot_grid(ca1, ca2, ca3, nrow=3)
cowplot::plot_grid(ma1, ma2, ma3, nrow=3)
dev.off()


forecast_dates <- as.character(seq.Date(as.Date("2020-12-07"), as.Date("2022-05-30"), by="7 days")) # c("2021-07-12", "2021-07-19", "2021-07-26")
states <- c("ca", "ma")
pdf(file = "plots/all-dates-data-and-forecasts.pdf", height=5, width=8)
for (state in states){
  for(forecast_date in forecast_dates){
    print(plot_case_hosp_forecasts(forecast_date = forecast_date, state=state))
  }
}
dev.off()