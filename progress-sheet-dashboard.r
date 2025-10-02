rm(list=ls())

library(dplyr)
library(quantregGrowth)
setwd("/Users/dwadhwa/Library/CloudStorage/OneDrive-WBG/SDG Atlas 2025/atlas-progress-measure")
#source("dashboard_indicator_data.R")

meta <- read.csv("/Users/dwadhwa/Library/CloudStorage/OneDrive-WBG/SDG Atlas 2025/atlas-progress-measure/output/meta_sheet.csv") |>
  collapse::fmutate(
    best = ifelse(more_is_better == 1,
                  "high",
                  "low")
  )

dashboard_final <- data.frame()
indicators <- na.omit(meta$indicator_wdi)

### SDG 2, 3, 4, 5, 6, 7, 9, 17 ###
for (indicator_wdi in indicators[c(2:7, 9, 12)]) {
  
  env <- new.env(parent = globalenv())
  env$indicator_wdi <- indicator_wdi
  sys.source("sdg_output_script.R", envir = env)   # run the script using this value
  
  df <- get("dashboard", envir = env)

  dashboard_final <- rbind(dashboard_final, df)
  
  }

### Merge in SDG 1, 8, and 10 data
sdg1 <- read_excel("dashboard_output_SDG1.xlsx", sheet = 1)
sdg8 <- read_excel("dashboard_output_SDG8.xlsx", sheet = 1)
sdg10 <- read_excel("dashboard_output_SDG10.xlsx", sheet = 1)

dashboard1810 <- sdg1 |>
  rbind(sdg8, sdg10) |>
  select(-c("reach_target", "pctl"))

dashboard_final <- dashboard_final |>
  rbind(dashboard1810) |>
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
# 
# write.csv(meta,
#           file="output/meta_sheet.csv", row.names = FALSE)
