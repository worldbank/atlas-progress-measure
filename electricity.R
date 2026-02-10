
# ____________________________ #
# SAMPLE WORKFLOW  ####
# ____________________________ #

# This script demonstrates how to:
# 1. Retrieve indicator data from WDI
# 2. Run the full model using {trackr} and optionally export results to Excel
# 3. Apply additional formatting steps to prepare the output for the SDG dashboard and export to Excel

# Preliminary operations
#rm(list=ls())

# Import packages & metadata
rm(list=ls())
library(trackr)
library(collapse)
library(readxl)
library(dplyr)
library(haven)

## !! NOTE: Add your own path ####
meta <- read.csv("/Users/dwadhwa/Library/CloudStorage/OneDrive-WBG/SDG Atlas 2025/atlas-progress-measure/output/meta_sheet.csv") |>
  collapse::fmutate(
    best = ifelse(more_is_better == 1,
                  "high",
                  "low")
  ) %>%
  rename(indicator_select = indicator_wdi) 

## Make sure you have the latest version installed ####
#devtools::install_github("RossanaTat/trackr")
setwd("/Users/dwadhwa/Library/CloudStorage/OneDrive-WBG/SDG Atlas 2025/atlas-progress-measure")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Life expectancy ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Input data __________ #
indicator_wdi <- "ELEC_SUP_PC"
load("input/electricity.Rda")
load("input/parameters.Rda")
data_wdi <- electricity %>%
  select(year, code, electricity)

# Params from wdi metadata  ----- #
# Change depending on which SDG, this example is SDG 2
meta_indicator <- meta %>%
  filter(indicator == indicator_wdi)
target         <- 10
best           <- meta_indicator$best
indicatorname  <- meta_indicator$indicatorname
indicator_sdg  <- meta_indicator$indicator_sdg
startyear_data <- 1950
endyear_data   <- as.numeric(meta_indicator$end_prog_eval)

# Track progress  _____ ####
progress_results <- trackr::track_progress(
  data           = data_wdi,
  indicator      = "electricity",
  code_col       = "code",
  year_col       = "year",
  startyear_data = startyear_data,
  endyear_data   = endyear_data,
  eval_from      = meta_indicator$start_prog_eval,
  eval_to        = meta_indicator$end_prog_eval,
  future         = TRUE,
  target_year    = 2030,
  speed          = TRUE,
  percentiles    = FALSE,
  sequence_pctl  = seq(20, 80, 20),
  sequence_speed = c(0.25, 0.5, 1, 2, 4),
  best           = best,
  #  min            = meta_indicator$min,
  #  max            = meta_indicator$max,
  support        = meta_indicator$support,
  granularity    = 0.01
)


#track_progress## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Prepare data for SDG dashboard ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## some outputs ------- ####
path_speed  <- progress_results$predicted_changes$path_speed
score_speed <- progress_results$scores$speed
typical_value <- progress_results$path_historical$speed

# Fromatting the data
data_wdi <- data_wdi %>%
  rename(y = electricity) %>%
  select(year, code, y) %>%
  mutate(target = target,
         best = best)

# Prepare data on when target was reached

if (!is.na(target)) {
  if (best=="low") {
    targetreachpast <- data_wdi |>
      filter(!is.na(y)) |>
      group_by(code) |>
      arrange(-year) |>
      mutate(maks = cummax(y))|>
      arrange(year) |>
      filter(y<=target & (lag(y)>target | is.na(lag(y))) & maks<=target) |>
      ungroup() |>
      select(code,
             year) |>
      rename("reach_target_year" = "year")
  }
  
  if (best=="high") {
    targetreachpast <- data_wdi |>
      filter(!is.na(y)) |>
      group_by(code) |>
      arrange(-year) |>
      mutate(mini = cummin(y)) |>
      arrange(year) |>
      filter(y>=target & (lag(y)<target | is.na(lag(y))) & mini>=target) |>
      ungroup() |>
      select(code,
             year) |>
      rename("reach_target_year" = "year")
  }
}

if (is.na(target)) {
  targetreachpast = data_wdi |> select(code) |> distinct()
}

year_target <- path_speed |>
  filter(y == target) |>
  select(time) |>
  as.numeric()

# keep only the typical end value for each country for the indicator
typical_value <- typical_value |>
  group_by(code) |>
  filter(speed == 1,
         year == max(year)) |>
  select(code, y_speed)

# Prepare speed data
dashboard_speed <- progress_results$scores$speed |>
  rename("start_year"        = "year_start",
         "end_year"          = "year",
         "end_value"         = "y_end",
         "start_value"       = "y",
         "speed"             = "score"
         #         "typical_end_value" = "y_speed"
  ) |>
  mutate(speed = if_else(start_value < target & best=="low" & !is.na(target),
                         NA,
                         speed),
         speed = if_else(start_value > target & best=="high" & !is.na(target),
                         NA,
                         speed),
         reach_target_year = round(if_else(speed > 0 & end_value > target & best=="low",
                                           (year_target-time_end)/speed + end_year,NA)),
         reach_target_year = round(if_else(speed > 0 & end_value < target & best=="high",
                                           (year_target-time_end)/speed + end_year,
                                           reach_target_year))) |>
  select(-evaluationperiod,
         -time_end,
         -time_start,
         -y_speed) |>
  merge(typical_value, by = "code") |>
  rename("typical_end_value" = "y_speed")

# Merge all
dashboard <- data_wdi |>
  # As a starter, keep last observation per group
  filter(!is.na(y)) |>
  group_by(code) |>
  arrange(year) |>
  slice(tail(row_number(),
             1)) |>
  ungroup() |>
  rename("end_year" = "year",
         "end_value" = "y") |>
  # Create some meta-data
  mutate(more_is_better = if_else(best=="high",1,0),
         target_value   = target,
         indicator_wdi  = indicator_wdi,
         indicatorname  = indicatorname,
         indicator_sdg  = indicator_sdg) |>
  # Merge with speed data
  joyn::joyn(dashboard_speed,
             match_type    = "1:1",
             by            = "code",
             update_values = TRUE,
             reportvar     = FALSE,
             verbose       = FALSE) |>
  joyn::joyn(targetreachpast,
             match_type = "1:1",
             by         = "code",
             update_NAs = TRUE,
             reportvar  = FALSE,
             verbose    = FALSE) |>
  mutate(reach_target = if_else(reach_target_year > end_year & !is.na(reach_target_year),
                                "future",
                                if_else((end_value>=target & best=="high") | (end_value<=target & best=="low"),
                                        "past",
                                        if_else(is.na(start_year),
                                                NA,
                                                "never"))),
         reach_target_year = if_else(reach_target == "past" & is.na(reach_target_year),
                                     startyear_data,
                                     reach_target_year)) |>
  joyn::joyn(progress_results$scores$pctl,
             by             = "code",
             match_type     = "1:1",
             reportvar      = FALSE,
             y_vars_to_keep = "score",
             verbose        = FALSE) |>
  rename("pctl"  = "score") |>
  mutate(pctl    = if_else(is.na(speed),
                           NA,
                           pctl))


dashboard <- dashboard[,c(8,6,7,4,5,2,14,13,1,3,11,10,9,12,15)]

writexl::write_xlsx(dashboard, "dashboard_output_electricity.xlsx")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Export to Excel ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## !! NOTE: Add your own path ####


### Progress results -output of {trackr} ####
#trackr::export_results_to_excel(res_list  = progress_results,
#                                file_path = "P:\\02.personal\\wb621604\\trackr_testing\\progress_results.xlsx")


### Dashboard Data ####
#writexl::write_xlsx(dashboard, "P:\\02.personal\\wb621604\\trackr_testing\\dashboard_output.xlsx")


