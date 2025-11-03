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
poverty <- read.csv("input/poverty-country.csv") %>%
  rename(iso3c = code,
         value = rate
         ) %>%
  mutate(variable = "SI.POV.DDAY")

# Load and process data for SDG 12
subsidy <- read_excel("input/Fossil Fuel Subsidy Map.xlsx", sheet = 1) %>%
  select(-c("countryname", "region", "incomelevel")) %>%
  melt(idvars = "iso3c") %>%
  rename(year = variable) %>%
  mutate(variable = "FF.SUB.GDP.ZS")

# Load and process data for SDG 14, filter out non-WB economies
cntries <- wbstats::wb_countries() |> filter(region != "Aggregates")
chloro <- read.csv("input/goal-14-input.csv") %>%
  select(ISO3Code, TimePeriod, Value, ) %>%
  rename(iso3c = ISO3Code,
         year = TimePeriod,
         value = Value) %>%
  mutate(variable = "EN_MAR_CHLDEV") %>%
  filter(iso3c %in% cntries$iso3c)

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

# Load SDG 1 data from input folder
d1reg  <- read.csv("input/poverty-region.csv") %>%
  mutate(iso3c = ifelse(region == "East Asia & Pacific", "EAS",
                        ifelse(region == "Europe & Central Asia", "ECS",
                               ifelse(region == "Latin America & Caribbean", "LCN",
                                      ifelse(region == "Middle East, North Africa, Afghanistan & Pakistan", "MEA",
                                             ifelse(region == "North America", "NAC",
                                                    ifelse(region == "South Asia", "SAS",
                                                           ifelse(region == "Sub-Saharan Africa", "SSF", NA
                                                                  )))))))) %>%
  select(iso3c, year, rate)

d1inc <- read.csv("input/poverty-incgroup.csv") %>%
  mutate(iso3c = ifelse(incgroup == "High income", "HIC",
                        ifelse(incgroup == "Upper middle income", "UMC",
                               ifelse(incgroup == "Lower middle income", "LMC",
                                      ifelse(incgroup == "Low income", "LIC", NA
                                             ))))) %>%
  select(iso3c, year, rate)
                        
d1 <- read.csv("input/poverty-global.csv") %>%
  mutate(iso3c = "WLD") %>%
  select(iso3c, year, rate) %>%
  rbind(d1reg, d1inc) %>%
  rename(value = rate) %>%
  mutate(variable = "SI.POV.DDAY")

# Load SDG 12 data from input folder
d12 <- read_excel("input/fossil_fuel_subsidy_global.xlsx", sheet = 1) %>%
  rename(value = `Fossil fuel subsidy (% of GDP)`,
         iso3c = region_code) %>%
  mutate(variable = "FF.SUB.GDP.ZS") %>%
  select(iso3c, year, value, variable)

# Load SDG 14 data from UNSDG website
d14 <- jsonlite::fromJSON(paste('https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=EN_MAR_CHLDEV&pageSize=10000',sep=""), flatten = TRUE)$data %>%
  as_tibble() %>%
  filter(str_detect(geoAreaName, "World")) %>%
  select(series, timePeriodStart, value) %>%
  mutate(iso3c = "WLD") %>%
  rename(year = timePeriodStart,
         variable = series)

values_agg <- data_agg %>%
  select(-iso2c, -country) %>%
  melt(id.vars = c("code", "year")) %>%
  rename(iso3c = code) %>%
  rbind(d14, d1, d12)

values_agg <- values_agg %>%
  merge(meta, by.x = "variable", 
                by.y = "indicator", 
                all.x = T) %>%
  select(variable, iso3c, year, value, indicator_sdg) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 2009)
  
write.csv(values_agg, "output/values_agg_sheet.csv", row.names = FALSE)

