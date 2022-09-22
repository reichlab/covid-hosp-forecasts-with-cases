library(tidyverse)
library(covidHubUtils)
theme_set(theme_bw())

# Parse model names ----
parse_model_names <- function(model) {
  name_segments <- str_split(model, "_")
  return(data.frame(
    case_type = sapply(name_segments, function(x) {
      if (x[1] %in% c("none", "report", "test")) x[1] else NA_character_
      }),
    case_source = sapply(name_segments, function(x) x[2]),
    smooth_case = sapply(name_segments, function(x) x[5]),
    p = sapply(name_segments, function(x) x[8]),
    d = sapply(name_segments, function(x) x[10]),
    P = sapply(name_segments, function(x) x[12]),
    D = sapply(name_segments, function(x) x[14])
  ))
}

# load forecasts ----
inc_hosp_targets <- paste(0:130, "day ahead inc hosp")

forecasts <- dplyr::bind_rows(
  load_forecasts(
    dates = seq.Date(from = as.Date("2020-12-07"),
                    to = as.Date("2021-06-07"),
                    by = 7),
    date_window_size = 6,
    locations = c("06", "25"),
    types = c("point", "quantile"),
    targets = inc_hosp_targets,
    source = "local_hub_repo",
    hub_repo_path = "forecasts/ca",
    data_processed_subpath = "",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")
  ),
  load_forecasts(
    dates = seq.Date(from = as.Date("2020-12-07"),
                    to = as.Date("2021-06-07"),
                    by = 7),
    date_window_size = 6,
    locations = c("06", "25"),
    types = c("point", "quantile"),
    targets = inc_hosp_targets,
    source = "local_hub_repo",
    hub_repo_path = "forecasts/ma",
    data_processed_subpath = "",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")
  ),
)

hub_forecasts <- load_forecasts(
  models = c("COVIDhub-baseline", "COVIDhub-ensemble"),
  dates = unique(forecasts$forecast_date),
  date_window_size = 6,
  locations = c("06", "25"),
  types = c("point", "quantile"),
  targets = inc_hosp_targets,
  source = "local_hub_repo",
  hub_repo_path = "../covid19-forecast-hub",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")
)

combined_forecasts <- dplyr::bind_rows(forecasts, hub_forecasts)

combined_forecasts %>%
  dplyr::filter(type == "quantile") %>%
  dplyr::count(location, model) %>%
  tidyr::pivot_wider(names_from = "location", values_from = "n") %>%
  as.data.frame()

# load truth data ----
truth_data <- dplyr::bind_rows(
  readr::read_csv("csv-data/CA-JHU-reportdate-hospitalizations-2022-07-22.csv") %>%
    dplyr::transmute(
      location = "06", target_end_date = date, value = inc,
      model = "Observed Data (HealthData)", target_variable = "inc hosp",
      location_name = "California", population = 39512223,
      geo_type = "state", geo_value = "ca", abbreviation = "CA",
      full_location_name = "California"
    ),
  readr::read_csv("csv-data/MA-JHU-reportdate-hospitalizations-2022-07-22.csv") %>%
    dplyr::transmute(
      location = "25", target_end_date = date, value = inc,
      model = "Observed Data (HealthData)", target_variable = "inc hosp",
      location_name = "Massachusetts", population = 6892503,
      geo_type = "state", geo_value = "ma", abbreviation = "MA",
      full_location_name = "Massachusetts"
    ),
)

truth_data <- truth_data %>%
  dplyr::filter(target_end_date >= "2020-10-01")

# calculate scores ----
scores <- covidHubUtils::score_forecasts(
  combined_forecasts,
  truth_data %>% filter(target_end_date <= "2021-06-12"),
  use_median_as_point = TRUE)

scores <- dplyr::bind_cols(
  scores,
  parse_model_names(scores$model)
)

mean_scores <- scores %>%
  group_by(location, model) %>%
  summarize(wis = mean(wis),
            mae = mean(abs_error),
            coverage_95 = mean(coverage_95)) %>%
  arrange(location, wis)

mean_scores <- dplyr::bind_cols(
  mean_scores,
  parse_model_names(mean_scores$model)
) %>%
  as.data.frame()
mean_scores

# plotting mean scores by horizon ----
mean_scores_by_hzn <- scores %>%
  dplyr::mutate(
    model_brief = dplyr::case_when(
      model %in% c("COVIDhub-4_week_ensemble", "COVIDhub-baseline") ~ model,
      TRUE ~ paste0(case_type, "_", case_source, "_smooth_case_", smooth_case)
    )
  ) %>%
  filter(!(model_brief %in%  c("COVIDhub-4_week_ensemble", "COVIDhub-baseline"))) %>%
  group_by(location, horizon, model_brief) %>%
  summarize(wis = mean(wis),
            mae = mean(abs_error),
            coverage_95 = mean(coverage_95)) %>%
  arrange(location, wis) %>%
  left_join(covidHubUtils::hub_locations, by= c("location" = "fips"))

p_wis_by_hzn <- ggplot(mean_scores_by_hzn, aes(x=as.numeric(horizon), y=wis, color=model_brief)) + 
  geom_line(aes(linetype = model_brief)) + 
  facet_wrap(.~location_name, ncol=1, scales="free_y") + 
  ylab("Average WIS") +
  scale_x_continuous(name="forecast horizon (days)", breaks=c(0, 7, 14, 21, 28)) +
  scale_color_manual(name = "model", 
                     labels = c("none_jhu-csse_smooth_case_False" = "HospOnly",
                                "report_dph_smooth_case_True" = "ReportCase-DPH",
                                "report_jhu-csse_smooth_case_True" = "ReportCase-CSSE",
                                "test_dph_smooth_case_True" = "TestCase-DPH"),
                     breaks = c("none_jhu-csse_smooth_case_False",
                                "report_jhu-csse_smooth_case_True", 
                                "report_dph_smooth_case_True", 
                                "test_dph_smooth_case_True"),
                     values = c("none_jhu-csse_smooth_case_False"= "darkgrey", 
                                "report_dph_smooth_case_True" = "#778A35", 
                                "report_jhu-csse_smooth_case_True" = "#E7B800", 
                                "test_dph_smooth_case_True" = "#FC4E07")) +
  scale_linetype_manual(name = "model", 
                        labels = c("none_jhu-csse_smooth_case_False" = "HospOnly",
                                   "report_dph_smooth_case_True" = "ReportCase-DPH",
                                   "report_jhu-csse_smooth_case_True" = "ReportCase-CSSE",
                                   "test_dph_smooth_case_True" = "TestCase-DPH"),
                        breaks = c("none_jhu-csse_smooth_case_False",
                                   "report_jhu-csse_smooth_case_True", 
                                   "report_dph_smooth_case_True", 
                                   "test_dph_smooth_case_True"),
                        values = c("none_jhu-csse_smooth_case_False"= 1, 
                                   "report_dph_smooth_case_True" = 3, 
                                   "report_jhu-csse_smooth_case_True" = 2, 
                                   "test_dph_smooth_case_True" = 4)) +
  theme(legend.position = c(0.03,0.97), legend.justification = c(0,1))

pdf("figures/validation_wis_by_horizon.pdf", height=6, width=8)
p_wis_by_hzn
dev.off()

# plot the forecasts for MA ----
models_to_plot <- c(
  "COVIDhub-ensemble", "COVIDhub-baseline",
  mean_scores %>%
    dplyr::filter(location == "25", !is.na(case_type)) %>%
    dplyr::group_by(case_type) %>%
    dplyr::slice_min(wis) %>%
    dplyr::pull(model)
)

forecasts_to_plot <- covidHubUtils::get_plot_forecast_data(
  forecast_data = combined_forecasts,
  truth_data = truth_data,
  models_to_plot = models_to_plot,
  horizons_to_plot = 28,
  quantiles_to_plot = c(0.025, 0.5, 0.975),
  locations_to_plot = "25",
  target_variable_to_plot = "inc hosp",
  hub = "US")

forecasts_to_plot <- forecasts_to_plot %>%
  dplyr::filter(!grepl("Observed Data", model))

# plot of predictive medians and 95% intervals, facetted by model ----
p <- ggplot() +
  geom_ribbon(
    data = forecasts_to_plot %>%
      dplyr::filter(!is.na(`Prediction Interval`)),
    mapping = aes(
      x = target_end_date,
      ymin = lower,
      ymax = upper,
      alpha = `Prediction Interval`,
      group = paste0(model, forecast_date)
    ),
    fill = "cornflowerblue"
  ) +
  scale_alpha_manual(
    values = c("95%" = 0.5)
  ) +
  geom_line(
    data = forecasts_to_plot %>%
      dplyr::filter(is.na(`Prediction Interval`)),
    mapping = aes(
      x = target_end_date,
      y = point,
      group = paste0(model, forecast_date)
    ),
    color = "blue"
  ) +
  geom_line(
    data = truth_data %>%
      dplyr::select(-model) %>%
      dplyr::filter(target_end_date < "2021-07-01", location == "25"),
    mapping = aes(x = target_end_date, y = value)
  ) +
  # ylim(0, 500) +
  facet_wrap( ~ model, scales = "free_y", ncol = 2)
p


# plot of predictive medians only (no intervals), facetted by forecast date ----
p <- ggplot() +
  # geom_ribbon(
  #   data = forecasts_to_plot %>%
  #     dplyr::filter(!is.na(`Prediction Interval`)),
  #   mapping = aes(
  #     x = target_end_date,
  #     ymin = lower,
  #     ymax = upper,
  #     alpha = `Prediction Interval`,
  #     group = paste0(model, forecast_date)
  #   ),
  #   fill = "cornflowerblue"
  # ) +
  # scale_alpha_manual(
  #   values = c("95%" = 0.5)
  # ) +
  geom_line(
    data = truth_data %>% dplyr::select(-model) %>% dplyr::filter(target_end_date < "2021-07-01", location == "25"),
    mapping = aes(x = target_end_date, y = value)
  ) +
  geom_line(
    data = forecasts_to_plot %>%
      dplyr::filter(is.na(`Prediction Interval`)),
    mapping = aes(
      x = target_end_date,
      y = point,
      color = model,
      group = paste0(model, forecast_date)
    ),
    size=1
  ) +
  # ylim(0, 500) +
  facet_wrap( ~ forecast_date, scales = "free_y")
p


# plot of mean WIS by forecast date ----
mean_scores_by_forecast_date <- scores %>%
  dplyr::filter(model %in% models_to_plot, location == "25") %>%
  dplyr::group_by(model, forecast_date) %>%
  dplyr::summarize(
    wis = mean(wis),
    mae = mean(abs_error)
  )
p <- ggplot(mean_scores_by_forecast_date) +
  geom_line(mapping = aes(x = forecast_date, y = wis, color = model, linetype = model)) +
  theme_bw()
p



# plot forecasts for CA ----
models_to_plot <- c(
  "COVIDhub-ensemble", "COVIDhub-baseline",
  mean_scores %>%
    dplyr::filter(location == "06", !is.na(case_type)) %>%
    dplyr::group_by(case_type) %>%
    # dplyr::slice_min(wis, n =3) %>%
    dplyr::slice_min(wis) %>%
    dplyr::pull(model)
)

forecasts_to_plot <- covidHubUtils::get_plot_forecast_data(
  forecast_data = combined_forecasts,
  truth_data = truth_data,
  # models_to_plot = unique(combined_forecasts$model),
  models_to_plot = models_to_plot,
  horizons_to_plot = 28,
  quantiles_to_plot = c(0.025, 0.5, 0.975),
  # quantiles_to_plot = c(0.025, 0.25, 0.5, 0.75, 0.975),
  locations_to_plot = "06",
  target_variable_to_plot = "inc hosp",
  hub = "US")

forecasts_to_plot <- forecasts_to_plot %>%
  dplyr::filter(!grepl("Observed Data", model))

# plot of predictive medians and 95% intervals, facetted by model ----
p <- ggplot() +
  geom_ribbon(
    data = forecasts_to_plot %>%
      dplyr::filter(!is.na(`Prediction Interval`)),
    mapping = aes(
      x = target_end_date,
      ymin = lower,
      ymax = upper,
      alpha = `Prediction Interval`,
      group = paste0(model, forecast_date)
    ),
    fill = "cornflowerblue"
  ) +
  scale_alpha_manual(
    values = c("95%" = 0.5)
  ) +
  geom_line(
    data = forecasts_to_plot %>%
      dplyr::filter(is.na(`Prediction Interval`)),
    mapping = aes(
      x = target_end_date,
      y = point,
      group = paste0(model, forecast_date)
    ),
    color = "blue"
  ) +
  geom_line(
    data = truth_data %>%
            dplyr::select(-model) %>%
            dplyr::filter(target_end_date < "2021-07-01", location == "06"),
    mapping = aes(x = target_end_date, y = value)
  ) +
  # ylim(0, 500) +
  facet_wrap( ~ model, scales = "free_y", ncol = 2)
p


# plot of predictive medians only (no intervals), facetted by forecast date ----
p <- ggplot() +
  # geom_ribbon(
  #   data = forecasts_to_plot %>%
  #     dplyr::filter(!is.na(`Prediction Interval`)),
  #   mapping = aes(
  #     x = target_end_date,
  #     ymin = lower,
  #     ymax = upper,
  #     alpha = `Prediction Interval`,
  #     group = paste0(model, forecast_date)
  #   ),
  #   fill = "cornflowerblue"
  # ) +
  # scale_alpha_manual(
  #   values = c("95%" = 0.5)
  # ) +
  geom_line(
    data = truth_data %>% dplyr::select(-model) %>% dplyr::filter(target_end_date < "2021-07-01", location == "06"),
    mapping = aes(x = target_end_date, y = value)
  ) +
  geom_line(
    data = forecasts_to_plot %>%
      dplyr::filter(is.na(`Prediction Interval`)),
    mapping = aes(
      x = target_end_date,
      y = point,
      color = model,
      group = paste0(model, forecast_date)
    ),
    size=1
  ) +
  # ylim(0, 500) +
  facet_wrap( ~ forecast_date, scales = "free_y")
p

# plot of mean WIS by forecast date ----
mean_scores_by_forecast_date <- scores %>%
  dplyr::mutate(
    model_brief = dplyr::case_when(
      model %in% c("COVIDhub-4_week_ensemble", "COVIDhub-baseline") ~ model,
      TRUE ~ paste0(case_type, "_", case_source, "_smooth_case_", smooth_case)
    )
  ) %>%
  filter(!(model_brief %in%  c("COVIDhub-4_week_ensemble", "COVIDhub-baseline"))) %>%
  dplyr::group_by(model_brief, forecast_date, location) %>%
  dplyr::summarize(
    wis = mean(wis),
    mae = mean(abs_error)
  ) %>%
  left_join(covidHubUtils::hub_locations, by= c("location" = "fips"))

p <- ggplot(mean_scores_by_forecast_date) +
  geom_point(mapping = aes(x = forecast_date, y = wis, color = model_brief, fill=model_brief, shape=model_brief)) +
  geom_line(mapping = aes(x = forecast_date, y = wis, color = model_brief)) +
  facet_wrap( ~ location_name, ncol = 1, scales = "free_y") +
  ylab("Average WIS") +
  scale_x_date(NULL, 
               date_breaks = "1 month", 
               date_labels = "%b '%y",
               expand = expansion(add=1)) +
  scale_color_manual(name = "model", 
                     labels = c("none_jhu-csse_smooth_case_False" = "HospOnly",
                                "report_dph_smooth_case_True" = "ReportCase-DPH",
                                "report_jhu-csse_smooth_case_True" = "ReportCase-CSSE",
                                "test_dph_smooth_case_True" = "TestCase-DPH"),
                     breaks = c("none_jhu-csse_smooth_case_False",
                                "report_jhu-csse_smooth_case_True", 
                                "report_dph_smooth_case_True", 
                                "test_dph_smooth_case_True"),
                     values = c("none_jhu-csse_smooth_case_False"= "darkgrey", 
                                "report_dph_smooth_case_True" = "#778A35", 
                                "report_jhu-csse_smooth_case_True" = "#E7B800", 
                                "test_dph_smooth_case_True" = "#FC4E07")) +
  scale_shape_manual(name = "model", 
                     labels = c("none_jhu-csse_smooth_case_False" = "HospOnly",
                                "report_dph_smooth_case_True" = "ReportCase-DPH",
                                "report_jhu-csse_smooth_case_True" = "ReportCase-CSSE",
                                "test_dph_smooth_case_True" = "TestCase-DPH"),
                     breaks = c("none_jhu-csse_smooth_case_False",
                                "report_jhu-csse_smooth_case_True", 
                                "report_dph_smooth_case_True", 
                                "test_dph_smooth_case_True"),
                     values = c("none_jhu-csse_smooth_case_False"= 15, 
                                "report_dph_smooth_case_True" = 16, 
                                "report_jhu-csse_smooth_case_True" = 17, 
                                "test_dph_smooth_case_True" = 18)) +
  scale_fill_manual(name = "model", 
                    labels = c("none_jhu-csse_smooth_case_False" = "HospOnly",
                               "report_dph_smooth_case_True" = "ReportCase-DPH",
                               "report_jhu-csse_smooth_case_True" = "ReportCase-CSSE",
                               "test_dph_smooth_case_True" = "TestCase-DPH"),
                    breaks = c("none_jhu-csse_smooth_case_False",
                               "report_jhu-csse_smooth_case_True", 
                               "report_dph_smooth_case_True", 
                               "test_dph_smooth_case_True"),
                    values = c("none_jhu-csse_smooth_case_False"= "darkgrey", 
                               "report_dph_smooth_case_True" = "#778A35", 
                               "report_jhu-csse_smooth_case_True" = "#E7B800", 
                               "test_dph_smooth_case_True" = "#FC4E07")) +
  theme_bw() +
  theme(axis.ticks.length.x = unit(0.5, "cm"), 
        axis.text.x = element_text(vjust = 7, hjust = -0.2),
        legend.position = c(0.03,0.97), legend.justification = c(0,1)) 

pdf("figures/validation_wis_by_horizon.pdf", height=6, width=8)
p_wis_by_fc_date
dev.off()

# final selection of models ----
sarix_mean_scores <- mean_scores %>%
    dplyr::filter(!is.na(case_type)) %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(rank = row_number())

# select best model within each combination of case_type and smooth_case options
sarix_mean_scores %>%
    dplyr::group_by(location, case_type, case_source, smooth_case) %>%
    dplyr::slice_min(wis) %>%
    dplyr::arrange(wis) %>%
    dplyr::select(-model)

# Output is:
# # A tibble: 12 × 12
# # Groups:   location, case_type, case_source, smooth_case [12]
#    location   wis   mae coverage_95 case_type case_source smooth_case p     d     P     D      rank
#    <chr>    <dbl> <dbl>       <dbl> <chr>     <chr>       <chr>       <chr> <chr> <chr> <chr> <int>
#  1 25        17.2  26.5       0.989 test      dph         True        1     0     1     1         1
#  2 25        17.4  25.1       0.984 test      dph         False       3     0     1     1         2
#  3 25        18.6  27.4       0.984 report    jhu-csse    True        4     1     0     0        16
#  4 25        18.7  28.2       0.970 report    jhu-csse    False       1     0     1     1        19
#  5 25        19.6  28.7       0.972 none      jhu-csse    False       1     0     1     1        32
#  6 06       105.  159.        0.977 test      dph         True        2     0     0     1         1
#  7 06       107.  173.        0.994 test      dph         False       4     1     1     0         3
#  8 06       114.  173.        0.997 report    jhu-csse    True        4     1     0     0        14
#  9 06       115.  172.        0.994 report    dph         True        4     1     0     0        16
# 10 06       122.  191.        0.997 report    jhu-csse    False       1     0     0     1        24
# 11 06       122.  199.        0.987 report    dph         False       1     1     2     0        25
# 12 06       125.  204.        0.992 none      jhu-csse    False       1     1     1     0        36
