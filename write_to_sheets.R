rm(list=ls())
library(googlesheets4)
library(tidyr)
library(dplyr)

# Collect all indicator values and aggregates
source("collect_indicators_values.R")
source("collect_indicators_aggregates.R")

# Run scripts to prepare progress for non-WDI indicators
source("prep/progress-electricity-supply.R")
source("prep/progress-life-expectancy.R")
source("prep/progress-sdg04-years-schooling.R")
source("prep/progress-women-empowerment-index.R")

# Calculate progress for WDI indicators, collect all progress data
source("collect_indicators_progress.R")

# Extend the metadata with data derived values
source("extend_meta_with_data.R")

values <- read.csv('output/values_sheet.csv')
write_sheet(values, ss = '14kDeXh0H6vjzldndSBN4p67CCNANHdm7EmqnwojcKh0', sheet = 'values')

agg <- read.csv('output/values_agg_sheet.csv')
write_sheet(agg, ss = '14kDeXh0H6vjzldndSBN4p67CCNANHdm7EmqnwojcKh0', sheet = 'aggregates')

progress <- read.csv('output/progress_sheet.csv')
write_sheet(progress, ss = '14kDeXh0H6vjzldndSBN4p67CCNANHdm7EmqnwojcKh0', sheet = 'progress')

#meta <- read.csv('output/meta_sheet.csv')
#write_sheet(meta, ss = '14kDeXh0H6vjzldndSBN4p67CCNANHdm7EmqnwojcKh0', sheet = 'meta')

meta.extended <- read.csv('output/meta_extended_sheet.csv')
write_sheet(meta.extended, ss = '14kDeXh0H6vjzldndSBN4p67CCNANHdm7EmqnwojcKh0', sheet = 'meta_extended')

# Indicator names and units for translations
to.translate <- select(meta.extended, indicator_sdg, indicator = indicatorname, indicator.short = indicatorshort, indicator.unit = unit)

to.translate.long <-  pivot_longer(to.translate,
    cols = 2:4,
    names_to = "key",
    values_to = "en"
  ) |>
  mutate(key = paste0(key, ".", indicator_sdg)) |>
  select(-indicator_sdg) |>
  mutate(es = NA, fr = NA, ar = NA, zh = NA, ja = NA)

write_sheet(to.translate.long, ss = '1KC-m96OtQx63iZMwf4q_dSc7oZ8gtpmkC-no7SChgco', sheet = "indicators.i18n")
