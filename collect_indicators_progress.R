# Calculate progress for WDI indicators, collect prepared progress data for non-WDI indicators
# Write to output/progress_sheet.csv

rm(list=ls())
library(dplyr)
library(quantregGrowth)
library(readxl)
#source("dashboard_indicator_data.R")

meta <- read.csv("input/meta_sheet.csv") |>
  collapse::fmutate(
    best = ifelse(more_is_better == 1,
                  "high",
                  "low")
  )

dashboard_final <- data.frame()
indicators <- na.omit(meta$indicator_wdi)

# These need to be added, but give an error: "SL.TLF.ACTI.FE.ZS", "SH.STA.MMRT"
wdind <- c("SN.ITK.DEFC.ZS", "SH.H2O.SMDW.ZS",
           "EG.ELC.ACCS.ZS", "IT.NET.USER.ZS", "IQ.SPI.OVRL",
           "SH.DYN.MORT", "SH.DYN.NMRT")

### SDG 2, 3, 5, 6, 7, 9, 17
for (indicator_wdi in wdind) {
  print(indicator_wdi)
  
  env <- new.env(parent = globalenv())
  env$indicator_wdi <- indicator_wdi
  sys.source("calculate_indicator_progress.R", envir = env)   # run the script using this value
  
  df <- get("dashboard", envir = env)

  dashboard_final <- rbind(dashboard_final, df)
}

### Merge in SDG 1, 4, 8, and 10 data
sdg1 <- read_excel("intermediate/new_dashboard_output_SDG1.xlsx", sheet = 1)
sdg4 <- read_excel("intermediate/dashboard_output_SDG4.xlsx", sheet = 1) %>%
  mutate(pctl = NA)
sdg8 <- read_excel("intermediate/dashboard_output_SDG8.xlsx", sheet = 1)
sdg10 <- read_excel("intermediate/dashboard_output_SDG10_14Jan.xlsx", sheet = 1)  %>%
  mutate(reach_target = NULL)
lifexp <- read_excel("intermediate/dashboard_output_lifeexpectancy.xlsx", sheet = 1)

dashboard14810 <- sdg1 |>
  rbind(sdg4, sdg8, sdg10) |>
  select(-c("pctl")) |>
  rbind(lifexp)

dashboard_final <- dashboard_final |>
  rbind(dashboard14810) |>
  rename(iso3c = code) |>
  select(-c("indicator_wdi", "more_is_better", "target_value", "target", "best", "indicatorname"))

write.csv(dashboard_final, file="output/progress_sheet.csv", row.names = FALSE)

#meta <- meta |>
#  select(-c("indicator_wdi", "more_is_better", "target_value", "target", "best", "indicatorname"))

#write.csv(dashboard_final, file="output/meta_sheet_output.csv", row.names = FALSE)

### SDG 8
# dashboard_final2 <- data.frame()
# for (indicator_wdi in indicators[8]) {
#   
#   env <- new.env(parent = globalenv())
#   env$indicator_wdi <- indicator_wdi
#   sys.source("sdg_output_script.R", envir = env)   # run the script using this value
#   
#   df <- get("dashboard", envir = env)
#   
#   dashboard_final2 <- rbind(dashboard_final2, df)
#   
# }
# 
# dashboard_final <- rbind(dashboard_final, dashboard_final2)
# 
# 
# ### SDG 1
# load("poverty_results.Rda")
# 
# #write.csv(dashboard_final,file="output/progress_data_dashboard.csv", row.names = FALSE)
# #dashboard_final <- read.csv("output/progress_data_dashboard.csv")
# 
# progress <- dashboard_final %>%
#   select(indicator_sdg, iso3c, speed, pctl, 
#          reach_target, reach_target_year, 
#          typical_end_value, start_year, end_year, start_value,
#          end_value) %>%
#   arrange(indicator_sdg)
# 
# meta <- dashboard_final %>%
#   select(sdg, indicatorname, indicator_wdi, 
#          indicator_sdg, more_is_better, 
#          target_value) %>%
#   distinct()
# 
# write.csv(progress,
#           file="output/progress_sheet.csv", row.names = FALSE)
