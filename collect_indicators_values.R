# Collect all indicator value data

rm(list=ls())
library(tidyverse)
library(quantregGrowth)
library(wbstats)
library(readxl)
library(haven)
library(reshape2)
#load("poverty.Rda")
#load("ghggdp.Rda")


### Full indicator list:
## From WDI (Atlas stories)
# 1 Gender: SL.TLF.ACTI.FE.ZS
# 2 Prosperity: SI.SPR.PGAP
# 3 Electricity: EG.ELC.ACCS.ZS
# 4 Internet: IT.NET.USER.ZS
# 5 SPI: IQ.SPI.OVRL

## Direct downloads (Atlas stories)
# 6 Education: HCI_EYRS
# 7 Water: wat_imp_prem_t
# 8 Water: wat_imp_qual_t
# 9 Water: wat_imp_av_t

## Direct downloads (Progress paper indicators)
# 10 Poverty: SI.POV.DDAY
# 11 Electricity: ELEC_SUP_PC
# 12 Gender: WOMEN.INDEX
# 13 Health: lifeexpectancy
# 14 Climate: carbon_intensity

## No progress but needs value indicator
# Climate
# Urban development
# Artificial intelligence

## No progress or values
# Overall progress

# read other data
meta <- read.csv("input/meta_sheet.csv") 

# Dashboard indicators list
wdind <- c("SL.TLF.ACTI.FE.ZS", 
           "EG.ELC.ACCS.ZS",  "IT.NET.USER.ZS", "EN.POP.SLUM.UR.ZS",
           "SI.SPR.PGAP", "EN.GHG.ALL.PC.CE.AR5",  "IQ.SPI.OVRL")

# Load and process data from WDI for SDG 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 15, 17
# Split into batches to avoid timeout
batch1 <- wbstats::wb_data(indicator = wdind[1:4], country="countries_only")
batch2 <- wbstats::wb_data(indicator = wdind[5:7], country="countries_only")

batch1.long <- batch1 |>
  select(-iso2c, -country) |>
  pivot_longer(cols = 3:6, names_to = 'variable', values_to = 'value')
batch2.long <- batch2 |>
  select(-iso2c, -country) |>
  pivot_longer(cols = 3:5, names_to = 'variable', values_to = 'value')

values <- rbind(batch1.long, batch2.long)

# Load and process data for AI chapter
ai <- read.csv("input/chatgpt.csv") %>%
  rename(iso3c = iso3,
         value = traffic_per_internet_user,
         date = year) %>%
  mutate(variable = "GPT.USE") %>%
  select(iso3c, date, variable, value)

# Load and process data for SDG 4
eyrs <- read_dta("input/EYS_data_update_2025 2.dta") %>%
  rename(iso3c = wbcode,
         value = eys_mf_fill,
         date = year) %>%
  mutate(variable = "HCI_EYRS") %>%
  select(iso3c, date, variable, value)

# Water SDG 6
# NO DATA IN REPO
water <- read_excel("~/Downloads/JMP_2025_WLD.xlsx", 
                    sheet = "wat") %>%
  select(iso3, year, wat_sm_t, wat_imp_prem_t, wat_imp_qual_t, wat_imp_av_t) %>%
  rename(iso3c = iso3,
         date = year) %>%
  melt(id = c("iso3c", "date"))

# Load and process data for SDG 1
poverty <- read.csv("input/poverty-country.csv") %>%
  rename(iso3c = code,
         value = rate,
         date = year
         ) %>%
  mutate(variable = "SI.POV.DDAY")

# Life expectancy at birth, total (years), 43.0.0
load("input/lifeexpectancy.Rda")
lifeexpectancy <- select(lifeexpectancy, -source) |>
  rename(iso3c = code, value = lifeexpectancy, date = year) |>
  mutate(variable = "lifeexpectancy") 

# Women political empowerment index, 45.0.0
load("input/gender.Rda")
gender <- gender %>%
  select(date = year, iso3c = code, value = gender) |>
  mutate(variable = "WOMEN.INDEX") 

# Electricity, 45.0.0
load("input/electricity.Rda")
electricity <- electricity %>%
  select(date = year, iso3c = code, value = electricity) |>
  mutate(variable = "ELEC_SUP_PC")

# Climate, 42.0.0
load("input/ghggdp.Rda")
climate <- ghggdp %>%
  select(date = year, iso3c = code, value = ghggdp) |>
  mutate(variable = "carbon_intensity") 

values <- values |>
  rbind(poverty) |>
  #rbind(water) |>
  rbind(eyrs) |>
  rbind(lifeexpectancy) |>
  rbind(gender) |>
  rbind(electricity) |>
  rbind(climate) |>
  rbind(ai)

values <- values |>
  merge(meta, by.x = "variable", 
                by.y = "indicator", 
                all.x = T) |>
#  mutate(value = round(value, rounding)) |>
  select(variable, iso3c, year = date, value, indicator_sdg) |>
  arrange(indicator_sdg, iso3c, year) |>
  mutate(year = as.numeric(year)) |>
  filter(year > 2009)

write.csv(values, "output/values_sheet.csv", row.names = FALSE, na = '')
