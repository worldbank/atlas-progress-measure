# Collect all indicator value data

rm(list=ls())
library(tidyverse)
library(quantregGrowth)
library(wbstats)
library(readxl)
library(haven)
#load("poverty.Rda")
#load("ghggdp.Rda")

# read other data
meta <- read.csv("input/meta_sheet.csv") 

# Dashboard indicators list
wdind <- c("SN.ITK.DEFC.ZS", "SL.TLF.ACTI.FE.ZS", "SH.H2O.SMDW.ZS",
           "EG.ELC.ACCS.ZS", "NY.GDP.PCAP.KD", "IT.NET.USER.ZS", 
           "SI.SPR.PGAP", "EN.POP.SLUM.UR.ZS", "EN.GHG.ALL.PC.CE.AR5",
           "ER.LND.PTLD.ZS", "IQ.SPI.OVRL",
           "SH.DYN.MORT", "SH.DYN.NMRT", "SH.STA.MMRT")

# Load and process data from WDI for SDG 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 15, 17
# Split into batches to avoid timeout
batch1 <- wbstats::wb_data(indicator = wdind[1:7], country="countries_only")
Sys.sleep(2)
batch2 <- wbstats::wb_data(indicator = wdind[8:14], country="countries_only")

data_wdi <- full_join(batch1, batch2, by = c('iso2c', 'iso3c', 'country', 'date')) |>
  rename("year" = "date")

values <- data_wdi |>
  select(-iso2c, -country) |>
  pivot_longer(cols = 3:16, names_to = 'variable', values_to = 'value')

# Load and process data for SDG 1
poverty <- read.csv("input/poverty-country.csv") %>%
  rename(iso3c = code,
         value = rate
         ) %>%
  mutate(variable = "SI.POV.DDAY")

# Load and process data for SDG 4
eyrs <- read_dta("input/EYS_data_update_2025 2.dta") %>%
  rename(iso3c = wbcode,
         value = eys_mf_fill) %>%
  mutate(variable = "HCI_EYRS") %>%
  select(iso3c, year, variable, value)

# Load and process data for SDG 12
subsidy <- read_excel("input/Fossil Fuel Subsidy Map.xlsx", sheet = 1) %>%
  select(-c("countryname", "region", "incomelevel")) %>%
  reshape2::melt(idvars = "iso3c") %>%
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

# Load and process data for AI chapter
ai <- read.csv("input/chatgpt.csv") %>%
  rename(iso3c = iso3,
         value = traffic_per_internet_user) %>%
  mutate(variable = "GPT.USE") %>%
  select(iso3c, year, variable, value)

# Load and process data for ID ownership chapter
countries <- values %>%
  distinct(iso3c) %>%
  pull(iso3c)

id <- read.csv("input/ID_OWN.csv") %>%
  filter(SEX == "_T",
         AGE == "_T",
         URBANISATION == "_T",
         COMP_BREAKDOWN_1 == "_T",
         COMP_BREAKDOWN_2 == "_T",
         COMP_BREAKDOWN_3 == "_T",
         REF_AREA %in% countries) %>%
  rename(iso3c = REF_AREA,
         year = TIME_PERIOD,
         value = OBS_VALUE) %>%
  mutate(variable = "ID_OWN") %>%
  select(iso3c, year, variable, value)

# Life expectancy at birth, total (years), 43.0.0
# Take from WDI?
load("input/lifeexpectancy.Rda")
lifeexpectancy <- select(lifeexpectancy, -source) |>
  rename(iso3c = code, value = lifeexpectancy) |>
  mutate(variable = "lifeexpectancy")

# Women political empowerment index, 45.0.0
load("input/gender.Rda")
gender <- select(gender, year, iso3c = code, value = gender) |>
  mutate(variable = "WOMEN.INDEX")

values <- values |>
  rbind(poverty) |>
  rbind(chloro) |>
  rbind(subsidy) |>
  rbind(eyrs) |>
  rbind(ai) |>
  rbind(id) |>
  rbind(lifeexpectancy) |>
  rbind(gender)

values <- values |>
  merge(meta, by.x = "variable", 
                by.y = "indicator", 
                all.x = T) |>
  mutate(value = round(value, rounding)) |>
  select(variable, iso3c, year, value, indicator_sdg) |>
  arrange(indicator_sdg, iso3c, year) |>
  mutate(year = as.numeric(year)) |>
  filter(year > 2009)

write.csv(values, "output/values_sheet.csv", row.names = FALSE, na = '')
