# Calculate progress for WDI indicators, collect prepared progress data for non-WDI indicators
# Write to output/progress_sheet.csv

rm(list=ls())
library(dplyr)
library(quantregGrowth)
library(readxl)
setwd("/Users/dwadhwa/Library/CloudStorage/OneDrive-WBG/SDG Atlas 2025/atlas-progress-measure/")

### Full indicator list:
## From WDI (Atlas stories)
# 1 Gender: SL.TLF.ACTI.FE.ZS
# 2 Prosperity: SI.SPR.PGAP
# 3 Electricity: EG.ELC.ACCS.ZS
# 4 Internet: IT.NET.USER.ZS
# 5 SPI: IQ.SPI.OVRL

# New
# 6 SN.ITK.DEFC.ZS
# 7 NY.GDP.PCAP.KD

## Direct downloads (Atlas stories)
# 8 Education: HCI_EYRS
# 9 Water: wat_imp_prem_t
# 10 Water: wat_imp_qual_t
# 11 Water: wat_imp_av_t

## Direct downloads (Progress paper indicators)
# 12 Poverty: SI.POV.DDAY
# 13 Electricity: ELEC_SUP_PC
# 14 Gender: WOMEN.INDEX
# 15 Health: lifeexpectancy
# 16 Climate: carbon_intensity

## No progress indicators
# Climate
# Urban development
# Overall progress
# Artificial intelligence
# ID ownership
# Fossil fuel subsidies
# Ocean plastic pollution

meta <- read.csv("input/meta_sheet.csv") |>
  collapse::fmutate(
    best = ifelse(more_is_better == 1,
                  "high",
                  "low")
  )

dashboard_final <- data.frame()
indicators <- na.omit(meta$indicator_wdi)

wdind <- c("EG.ELC.ACCS.ZS", "SL.TLF.ACTI.FE.ZS",
           "IT.NET.USER.ZS", "IQ.SPI.OVRL", "SI.SPR.PGAP",
           "SN.ITK.DEFC.ZS", "NY.GDP.PCAP.KD")

### SDG 2, 5, 7, 8, 9, 17
for (indicator_wdi in wdind) {
  print(indicator_wdi)
  
  env <- new.env(parent = globalenv())
  env$indicator_wdi <- indicator_wdi
  sys.source("scripts/calculate_indicator_progress.R", envir = env)   # run the script using this value
  
  df <- get("dashboard", envir = env)

  dashboard_final <- rbind(dashboard_final, df)
}

### Merge in SDG 1, 4, 10, life expectancy, gender, ghggdp, water components
### data
sdg1 <- read_excel("intermediate/dashboard_output_poverty.xlsx", sheet = 1)
sdg4 <- read_excel("intermediate/dashboard_output_SDG4.xlsx", sheet = 1) %>%
  mutate(pctl = NULL)
#sdg8 <- read_excel("intermediate/dashboard_output_SDG8.xlsx", sheet = 1)
#sdg10 <- read_excel("intermediate/dashboard_output_SDG10_14Jan.xlsx", sheet = 1)  %>%
#  mutate(reach_target = NULL)
lifexp <- read_excel("intermediate/dashboard_output_lifeexpectancy.xlsx", sheet = 1)
gender <- read_excel("intermediate/dashboard_output_gender.xlsx", sheet = 1)
ghggdp <- read_excel("intermediate/dashboard_output_ghggdp.xlsx", sheet = 1)
sdg6 <- read_excel("intermediate/dashboard_output_water_components.xlsx", sheet = 1) |>
  # All 3 subindicators are mapped to 6.1.1 in the data
  mutate(indicator_sdg = case_when(
    indicator_wdi == 'wat_imp_prem_t' ~ '61.1.0',
    indicator_wdi == 'wat_imp_qual_t' ~ '61.2.0',
    indicator_wdi == 'wat_imp_av_t' ~ '61.3.0',
  ))
electricity <- read_excel("intermediate/dashboard_output_electricity.xlsx", sheet = 1)

dashboard14810 <- sdg1 |>
  rbind(sdg4) |>
  rbind(lifexp) |>
  rbind(gender) |>
  rbind(ghggdp) |>
  rbind(sdg6) |>
  rbind(electricity)

dashboard_final <- dashboard_final |>
  rbind(dashboard14810) |>
  rename(iso3c = code) |>
  select(-c("indicator_wdi", "more_is_better", "target_value", "target", "best", "indicatorname")) |>
  # some empty rows are introduced somewhere, remove them
  filter(!is.na(indicator_sdg))

write.csv(dashboard_final, file="output/progress_sheet.csv", row.names = FALSE)


