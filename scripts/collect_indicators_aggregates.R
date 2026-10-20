library(tidyverse)
library(haven)
library(readxl)

# Dashboard indicators list
wdind <- c("SL.TLF.ACTI.FE.ZS", 
           "EG.ELC.ACCS.ZS",  "IT.NET.USER.ZS", "EN.POP.SLUM.UR.ZS",
           "SI.SPR.PGAP", "EN.GHG.ALL.PC.CE.AR5",  "IQ.SPI.OVRL",
           "SN.ITK.DEFC.ZS", "NY.GDP.PCAP.KD")

meta <- read.csv("input/meta_sheet.csv") 

#### Aggregate values
aggs <- c("WLD", "HIC", "UMC", "LMC", "LIC", "SSF", "LCN", "SAS", "MEA", "NAC", "EAS", "ECS")

# Load data from WDI
# Split into batches to avoid timeout
data_agg <- wbstats::wb_data(indicator = wdind, country = aggs) |>
  select(-iso2c, -country) |>
  rename("year" = "date") |>
  pivot_longer(cols = 3:11, names_to = 'variable', values_to = 'value') |>
  mutate(iso3c = case_when(
    iso3c == "XD" ~ "HIC",
    iso3c == "XM" ~ "LIC",
    iso3c == "XN" ~ "LMC",
    iso3c == "XT" ~ "UMC",
    TRUE ~ iso3c
  ))

# Load SDG 1 data from input folder
d1reg  <- read.csv("input/poverty-region.csv") |>
  mutate(iso3c = case_when(
    region == "East Asia & Pacific" ~ "EAS",
    region == "Europe & Central Asia" ~ "ECS",
    region == "Latin America & Caribbean" ~ "LCN",
    region == "Middle East, North Africa, Afghanistan & Pakistan" ~ "MEA",
    region == "North America" ~ "NAC",
    region == "South Asia" ~ "SAS",
    region == "Sub-Saharan Africa" ~ "SSF",
    TRUE ~ NA_character_
  )) |>
  select(iso3c, year, rate)

d1inc <- read.csv("input/poverty-incgroup.csv") |>
    mutate(iso3c = case_when(
      incgroup == "High income"         ~ "HIC",
      incgroup == "Upper middle income" ~ "UMC",
      incgroup == "Lower middle income" ~ "LMC",
      incgroup == "Low income"          ~ "LIC",
      TRUE                              ~ NA_character_
    )) |>
    select(iso3c, year, rate)
                        
d1 <- read.csv("input/poverty-global.csv") %>%
  mutate(iso3c = "WLD") %>%
  select(iso3c, year, rate) %>%
  rbind(d1reg, d1inc) %>%
  rename(value = rate) %>%
  mutate(variable = "SI.POV.DDAY")

values_agg <- rbind(data_agg, d1)

values_agg <- values_agg |>
  merge(meta, by.x = "variable", 
                by.y = "indicator", 
                all.x = T) |>
  mutate(value = round(value, rounding)) |>
  select(variable, iso3c, year, value, indicator_sdg) |>
  mutate(year = as.numeric(year)) |>
  filter(year > 2009)
  
write.csv(values_agg, "output/values_agg_sheet.csv", row.names = FALSE, na = '')
