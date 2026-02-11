library(dplyr)
library(googlesheets4)

prog <- read.csv('output/progress_sheet.csv')

indicators <- group_by(prog, indicator_sdg) |>
  mutate(diff = end_value - start_value) |>
  summarise(
    minStartYear = min(start_year, na.rm = TRUE),
    maxEndYear = max(end_year, na.rm = TRUE),
    startValueMin = min(start_value, na.rm = TRUE),
    startValueMax = max(start_value, na.rm = TRUE),
    endValueMin = min(end_value, na.rm = TRUE),
    endValueMax = max(end_value, na.rm = TRUE),
    diffMin = min(diff, na.rm = TRUE),
    diffMax = max(diff, na.rm = TRUE),
    speedMin = min(speed, na.rm = TRUE),
    speedMax = max(speed, na.rm = TRUE),
    reachTargetYearMin = min(reach_target_year, na.rm = TRUE),
    reachTargetYearMax = max(reach_target_year, na.rm = TRUE)
  )

values <- read.csv('output/values_sheet.csv')

# In case no progress data is available
year_extents <- filter(values, !(indicator_sdg %in% indicators$indicator_sdg)) |>
  filter(!is.na(value)) |>
  filter(year > 2014) |>
  group_by(indicator_sdg) |>
  summarise(
    minStartYear = min(year, na.rm = TRUE),
    maxEndYear = max(year, na.rm = TRUE),
  )

values_summarised <- filter(values, !is.na(value)) |>
  group_by(indicator_sdg, year) |>
  summarise(
    minValue = min(value, na.rm = TRUE),
    maxValue = max(value, na.rm = TRUE)
  )

extents_values <- left_join(year_extents, values_summarised, by = c("indicator_sdg", "minStartYear" = "year")) |>
  rename(startValueMin = minValue, startValueMax = maxValue) |>
  left_join(values_summarised, by = c("indicator_sdg", "maxEndYear" = "year")) |>
  rename(endValueMin = minValue, endValueMax = maxValue)

# diffs
diffs <- filter(values, year > 2014) |>
  filter(!is.na(value)) |>
  group_by(indicator_sdg, iso3c) |>
  summarise(
    diff = value[year == max(year)] - value[year == min(year)]
  ) |>
  summarise(diffMin = min(diff, na.rm = TRUE), diffMax = max(diff, na.rm = TRUE))

extents_values_diffs <- left_join(extents_values, diffs, by = "indicator_sdg") |>
  mutate(speedMin = NA_real_, speedMax = NA_real_, reachTargetYearMin = NA_real_, reachTargetYearMax = NA_real_)

# SHOULD THESE BE THE EXTENT OF THE FULL PERIOD, OR ONLY FROM THE MOST RECENT VALUES?
value_extents <- group_by(values, indicator_sdg) |>
  summarise(
    valueMin = min(value, na.rm = TRUE),
    valueMax = max(value, na.rm = TRUE))

all_indicator_data <- rbind(indicators, extents_values_diffs) |>
  left_join(value_extents, by = "indicator_sdg") |>
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA_real_, .)))

meta <- read.csv('output/meta_sheet.csv') |>
  select(-indicatorname, -indicatorshort, -unit) |>
  mutate(indicator_360 = ifelse(!is.na(indicator_wdi), paste0('WB_WDI_', gsub("\\.", "_", indicator_wdi)), indicator_wdi))

# Add pillars mapping
mapping <- read.csv("input/goals-pillars-mapping.csv")

# Number of countries that reached the target
reached.target <- left_join(prog, meta, by = "indicator_sdg") |>
  group_by(indicator_sdg) |>
  summarise(reached_target = sum(if_else(more_is_better == 1, end_value >= target_value, end_value <= target_value)))

meta.extended <- left_join(meta, all_indicator_data, by = 'indicator_sdg') |>
  left_join(reached.target, by = 'indicator_sdg') |>
  left_join(mapping, by = "sdg")

write.csv(meta.extended, file = 'output/meta_extended_sheet.csv', row.names = FALSE, na = '')
