rm(list=ls())
library(tidyverse)
library(reshape2)
library(quantregGrowth)
library(wbstats)
library(readxl)
setwd("/Users/dwadhwa/Library/CloudStorage/OneDrive-WBG/SDG Atlas 2025/atlas-progress-measure")
#load("poverty.Rda")
#load("ghggdp.Rda")

# read other data
meta <- read.csv("output/meta_sheet.csv") 
progress <- read.csv("output/progress_sheet.csv")

# Dashboard indicators list
dashboard_indicators <- meta %>%
  filter(sdg != 1 & sdg != 12 & sdg != 14 & !is.na(indicator))

# Load and process data from WDI for SDG 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 15, 17
data_wdi <- wbstats::wb_data(indicator = dashboard_indicators$indicator,
                             country="countries_only") |>
  rename("year" = "date","code" = "iso3c") 

values <- data_wdi %>%
  select(-iso2c, -country) %>%
  melt(id.vars = c("code", "year")) %>%
  rename(iso3c = code) 

# Load and process data for SDG 1
poverty <- read.csv("poverty-country.csv") %>%
  rename(iso3c = code,
         value = rate
         ) %>%
  mutate(variable = "SI.POV.DDAY")

# Load and process data for SDG 12
subsidy <- read_excel("Fossil Fuel Subsidy Map.xlsx", sheet = 1) %>%
  select(-c("countryname", "region", "incomelevel")) %>%
  melt(idvars = "iso3c") %>%
  rename(year = variable) %>%
  mutate(variable = "FF.SUB.GDP.ZS")

# Load and process data for SDG 14
chloro <- read.csv("goal-14-input.csv") %>%
  select(ISO3Code, TimePeriod, Value, ) %>%
  rename(iso3c = ISO3Code,
         year = TimePeriod,
         value = Value) %>%
  mutate(variable = "EN_MAR_CHLDEV")

values <- values |>
  rbind(poverty) |>
  rbind(chloro) |>
  rbind(subsidy)

values <- values |>
  merge(meta, by.x = "variable", 
                by.y = "indicator", 
                all.x = T) |>
  select(variable, iso3c, year, value, indicator_sdg)  %>%
  arrange(indicator_sdg, iso3c, year) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 2009)

write.csv(values, "output/values_sheet.csv", row.names = FALSE)

#### Aggregate values
# Load data from WDI
data_agg <- wbstats::wb_data(indicator = dashboard_indicators$indicator,
                             country=c("WLD", "HIC", "UMC", "LMC", "LIC", "SSF",
                                       "LCN", "SAS", "MEA", "NAC", "EAS", "ECS")) |>
  rename("year" = "date","code" = "iso3c") |>
  mutate(code = ifelse(country == "High income", "HIC", code),
         code = ifelse(country == "Low income", "LIC", code),
         code = ifelse(country == "Lower middle income", "LMC", code),
         code = ifelse(country == "Upper middle income", "UMC", code))

values_agg <- data_agg %>%
  select(-iso2c, -country) %>%
  melt(id.vars = c("code", "year")) %>%
  rename(iso3c = code) 

values_agg <- values_agg %>%
  merge(meta, by.x = "variable", 
                by.y = "indicator", 
                all.x = T) %>%
  select(variable, iso3c, year, value, indicator_sdg) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 2009)
  
write.csv(values_agg, "output/values_agg_sheet.csv", row.names = FALSE)

# Calculate aggregates for SDG 1, 12 and 14
# TBD