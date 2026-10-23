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
# 6 Urban: EN.POP.SLUM.UR.ZS
# 7 Climate: EN.GHG.ALL.PC.CE.AR5

# New
# 8 SN.ITK.DEFC.ZS
# 9 NY.GDP.PCAP.KD

## Direct downloads (Atlas stories)
# 10 Education: HCI_EYRS
# 11 Water: wat_imp_prem_t
# 12 Water: wat_imp_qual_t
# 13 Water: wat_imp_av_t
# 14 Artificial intelligence: GPT.USE

# New
# 12 ID ownership: ID_OWN
# 13 Fossil fuel subsidies: FFSUBS
# 14 Oceans: 

## Direct downloads (Progress paper indicators)
# 15 Poverty: SI.POV.DDAY
# 16 Electricity: ELEC_SUP_PC
# 17 Gender: WOMEN.INDEX
# 18 Health: lifeexpectancy
# 19 Climate: carbon_intensity

## No progress but needs value indicator
# Climate

## No progress or values
# Overall progress

# read other data
meta <- read.csv("input/meta_sheet.csv") 

# Dashboard indicators list
wdind <- c("SL.TLF.ACTI.FE.ZS", 
           "EG.ELC.ACCS.ZS",  "IT.NET.USER.ZS", "EN.POP.SLUM.UR.ZS",
           "SI.SPR.PGAP", "EN.GHG.ALL.PC.CE.AR5",  "IQ.SPI.OVRL",
           "SN.ITK.DEFC.ZS", "NY.GDP.PCAP.KD")

# Load and process data from WDI for SDG 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 15, 17
# Split into batches to avoid timeout
batch1 <- wbstats::wb_data(indicator = wdind[1:5], country="countries_only")
batch2 <- wbstats::wb_data(indicator = wdind[6:9], country="countries_only")

batch1.long <- batch1 |>
  select(-iso2c, -country) |>
  pivot_longer(cols = 3:7, names_to = 'variable', values_to = 'value')
batch2.long <- batch2 |>
  select(-iso2c, -country) |>
  pivot_longer(cols = 3:6, names_to = 'variable', values_to = 'value')

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

# Load and process data for ID ownership
id <- read.csv("input/ID_OWN_WIDEF.csv") %>%
  filter(SEX == "_T",
         AGE == "_T",
         URBANISATION == "_T",
         COMP_BREAKDOWN_1 == "_T",
         COMP_BREAKDOWN_2 == "_T",
         COMP_BREAKDOWN_3 == "_T") %>%
  select(REF_AREA, X2017, X2021, X2024) %>%
  pivot_longer(cols = c(X2017, X2021, X2024), names_to = "date", values_to = "value") %>%
  rename(iso3c = REF_AREA) %>%
  mutate(variable = "ID_OWN",
         date = as.numeric(gsub("X", "", date))) %>%
  select(iso3c, date, variable, value)
  
# Load and process data for Fossil fuel subsidies
subsidy <- read_excel("input/fossil_fuel_subsidies.xlsx", sheet = "fig3") %>%
   rename(iso3c = `Country code`,
          value = `Fossil fuel subsidy in 2024 ($ per capita)`) %>%
   mutate(variable = "subsidy",
          date = 2024) %>%
   select(iso3c, date, variable, value)
  
# Load and process data for plastic pollution
plastic <- read.csv("input/plastic-waste-pollution.csv") %>%
  filter(Code != "") %>%
  rename(iso3c = Code,
        value = Plastic.pollution.per.capita..kg.,
        date = Year) %>%
   mutate(variable = "plastic") %>%
   select(iso3c, date, variable, value)

# Water SDG 6
water <- read_excel("input/JMP_2025_WLD.xlsx", 
                    sheet = "wat") %>%
  select(iso3, year, wat_imp_prem_t, wat_imp_qual_t, wat_imp_av_t) %>%
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
  rbind(water) |>
  rbind(eyrs) |>
  rbind(lifeexpectancy) |>
  rbind(gender) |>
  rbind(electricity) |>
  rbind(climate) |>
  rbind(ai) |>
  rbind(subsidy) |>
  rbind(plastic) |>
  rbind(id)
  
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
