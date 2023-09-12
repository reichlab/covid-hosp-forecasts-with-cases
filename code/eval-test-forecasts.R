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
  dates = seq.Date(from = as.Date("2021-06-14"),
                   to = as.Date("2022-05-30"),
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
    dates = seq.Date(from = as.Date("2021-06-14"),
                   to = as.Date("2022-05-30"),
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
  )
)

hub_forecasts <- load_forecasts(
  models = c("COVIDhub-baseline", "COVIDhub-4_week_ensemble"),
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
  dplyr::count(location, model, forecast_date) %>%
  as.data.frame()

combined_forecasts %>%
  dplyr::filter(type == "quantile") %>%
  dplyr::count(location, model) %>%
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
  truth_data,
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
  geom_line() + 
  geom_point(aes(shape = model_brief)) + 
  facet_wrap(.~fct_rev(location_name), ncol=1, scales="free_y") + 
  ylab("Mean WIS") +
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
                     values = c("none_jhu-csse_smooth_case_False"= "#ff7f00", 
                                "report_dph_smooth_case_True" = "#6a3d9a", 
                                "report_jhu-csse_smooth_case_True" = "#1f78b4", 
                                "test_dph_smooth_case_True" = "#33a02c")) +
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
  theme(legend.position = c(0.03,0.97), legend.justification = c(0,1))

pdf("figures/test_wis_by_horizon.pdf", height=6, width=8)
p_wis_by_hzn
dev.off()

# plot the forecasts ----
pdf("figures/pred_dists_by_model.pdf", height = 12, width = 16)
for (loc in c("06", "25")) {
  loc_name <- truth_data %>%
    dplyr::select(-model) %>%
    dplyr::filter(location == loc) %>%
    dplyr::pull(location_name)

  forecasts_to_plot <- covidHubUtils::get_plot_forecast_data(
    forecast_data = combined_forecasts,
    truth_data = truth_data,
    models_to_plot = unique(combined_forecasts$model),
    horizons_to_plot = 28,
    quantiles_to_plot = c(0.025, 0.5, 0.975),
    # quantiles_to_plot = c(0.025, 0.25, 0.5, 0.75, 0.975),
    locations_to_plot = loc,
    target_variable_to_plot = "inc hosp",
    hub = "US")

  forecasts_to_plot <- forecasts_to_plot %>%
    dplyr::filter(!grepl("Observed Data", model))

  # plot of predictive medians and 95% intervals, facetted by model
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
      data = truth_data %>% dplyr::select(-model) %>% filter(location == loc),
      mapping = aes(x = target_end_date, y = value)
    ) +
    ggtitle(loc_name) +
    # ylim(0, 500) +
    facet_wrap( ~ model, scales = "free_y", ncol = 2)

  print(p)
}
dev.off()


# plot of predictive medians only (no intervals), facetted by forecast date ----
pdf("figures/pred_medians_by_date.pdf", height = 48, width = 16)
for (loc in c("06", "25")) {
  loc_name <- covidData::fips_codes %>%
    dplyr::filter(location == loc) %>%
    dplyr::pull(location_name)

  forecasts_to_plot <- covidHubUtils::get_plot_forecast_data(
    forecast_data = combined_forecasts,
    truth_data = truth_data,
    models_to_plot = unique(combined_forecasts$model),
    horizons_to_plot = 28,
    quantiles_to_plot = c(0.025, 0.5, 0.975),
    # quantiles_to_plot = c(0.025, 0.25, 0.5, 0.75, 0.975),
    locations_to_plot = loc,
    target_variable_to_plot = "inc hosp",
    hub = "US")

  forecasts_to_plot <- forecasts_to_plot %>%
    dplyr::filter(!grepl("Observed Data", model))

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
      data = truth_data %>% dplyr::select(-model) %>% dplyr::filter(location == loc),
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
    ggtitle(loc_name) +
    # ylim(0, 500) +
    facet_wrap( ~ forecast_date, scales = "free_y", ncol = 3) +
    theme(legend.position = "bottom")
  print(p)
}
dev.off()


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
    mean_wis = mean(wis),
    med_wis = median(wis),
    mae = mean(abs_error)
  ) %>%
  left_join(covidHubUtils::hub_locations, by= c("location" = "fips"))

p_mean <- ggplot(mean_scores_by_forecast_date) +
  geom_point(mapping = aes(x = forecast_date, y = mean_wis, color = model_brief, fill=model_brief, shape=model_brief)) +
  geom_line(mapping = aes(x = forecast_date, y = mean_wis, color = model_brief)) +
  facet_wrap( ~ fct_rev(location_name), ncol = 1, scales = "free_y") +
  ylab("Mean WIS") +
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
                     values = c("none_jhu-csse_smooth_case_False"= "#ff7f00", 
                                "report_dph_smooth_case_True" = "#6a3d9a", 
                                "report_jhu-csse_smooth_case_True" = "#1f78b4", 
                                "test_dph_smooth_case_True" = "#33a02c")) +
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
                    values = c("none_jhu-csse_smooth_case_False"= "#ff7f00", 
                               "report_dph_smooth_case_True" = "#6a3d9a", 
                               "report_jhu-csse_smooth_case_True" = "#1f78b4", 
                               "test_dph_smooth_case_True" = "#33a02c")) +
  theme_bw() +
  theme(axis.ticks.length.x = unit(0.5, "cm"), 
        axis.text.x = element_text(vjust = 7, hjust = -0.2),
        legend.position = c(0.03,0.97), legend.justification = c(0,1)) 

pdf("figures/test_mean_wis_by_fc_date.pdf", height = 6, width = 8)
p_mean
dev.off()


p_median <- ggplot(mean_scores_by_forecast_date) +
  geom_point(mapping = aes(x = forecast_date, y = med_wis, color = model_brief, fill=model_brief, shape=model_brief)) +
  geom_line(mapping = aes(x = forecast_date, y = med_wis, color = model_brief)) +
  facet_wrap( ~ fct_rev(location_name), ncol = 1, scales = "free_y") +
  ylab("Median WIS") +
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
                     values = c("none_jhu-csse_smooth_case_False"= "#ff7f00", 
                                "report_dph_smooth_case_True" = "#6a3d9a", 
                                "report_jhu-csse_smooth_case_True" = "#1f78b4", 
                                "test_dph_smooth_case_True" = "#33a02c")) +
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
                    values = c("none_jhu-csse_smooth_case_False"= "#ff7f00", 
                               "report_dph_smooth_case_True" = "#6a3d9a", 
                               "report_jhu-csse_smooth_case_True" = "#1f78b4", 
                               "test_dph_smooth_case_True" = "#33a02c")) +
  theme_bw() +
  theme(axis.ticks.length.x = unit(0.5, "cm"), 
        axis.text.x = element_text(vjust = 7, hjust = -0.2),
        legend.position = c(0.03,0.97), legend.justification = c(0,1)) 

pdf("figures/test_median_wis_by_fc_date.pdf", height = 6, width = 8)
p_median
dev.off()


# plot of mean WIS by forecast date and horizon ----
mean_scores_by_forecast_date_horizon <- scores %>%
  dplyr::mutate(
    model_brief = dplyr::case_when(
      model %in% c("COVIDhub-4_week_ensemble", "COVIDhub-baseline") ~ model,
      TRUE ~ paste0(case_type, "_", case_source, "_smooth_case_", smooth_case)
    )
  ) %>%
  filter(!(model_brief %in%  c("COVIDhub-4_week_ensemble", "COVIDhub-baseline"))) %>%
  dplyr::group_by(model_brief, forecast_date, horizon, location) %>%
  dplyr::summarize(
    wis = mean(wis),
    mae = mean(abs_error)
  )

ggplot(mean_scores_by_forecast_date_horizon) +
  geom_point(mapping = aes(x = forecast_date, y = wis, color = model_brief, fill=model_brief, shape=model_brief)) +
  geom_line(mapping = aes(x = forecast_date, y = wis, color = model_brief, linetype = model_brief)) +
  facet_grid( horizon ~ location) +
  theme_bw()
