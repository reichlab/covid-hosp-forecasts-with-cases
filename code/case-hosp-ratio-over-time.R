library(covidcast)

tmp_ma_cases <- covidcast_signal(data_source = "jhu-csse", 
                                 signal="confirmed_7dav_incidence_num", 
                                 geo_type="state", geo_values="ma") |> 
  rename(cases=value) |> 
  select(time_value, cases)
tmp_ma_hosps <- covidcast_signal(data_source = "hhs", 
                                 signal="confirmed_admissions_covid_1d_7dav", 
                                 geo_type="state", geo_values="ma") |> 
  rename(hosps = value) |> 
  select(time_value, hosps)

tmp_all <- left_join(tmp_ma_cases, tmp_ma_hosps) |> 
  mutate(cases_per_hosp = cases/hosps)

ggplot(tmp_all, aes(x=time_value, y=cases_per_hosp)) + geom_point()
