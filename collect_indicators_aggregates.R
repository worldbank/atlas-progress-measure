library(tidyverse)

# Dashboard indicators list
wdind <- c("SN.ITK.DEFC.ZS", "SL.TLF.ACTI.FE.ZS", "SH.H2O.SMDW.ZS",
           "EG.ELC.ACCS.ZS", "NY.GDP.PCAP.KD", "IT.NET.USER.ZS", 
           "SI.SPR.PGAP", "EN.POP.SLUM.UR.ZS", "EN.GHG.ALL.PC.CE.AR5",
           "ER.LND.PTLD.ZS", "IQ.SPI.OVRL",
           "SH.DYN.MORT", "SH.DYN.NMRT", "SH.STA.MMRT")

#### Aggregate values
aggs <- c("WLD", "HIC", "UMC", "LMC", "LIC", "SSF", "LCN", "SAS", "MEA", "NAC", "EAS", "ECS")

# Load data from WDI
# Split into batches to avoid timeout
agg_batch1 <- wbstats::wb_data(indicator = wdind[1:7], country = aggs)
Sys.sleep(2)
agg_batch2 <- wbstats::wb_data(indicator = wdind[8:14], country = aggs)

data_agg <- full_join(agg_batch1, agg_batch2, by = c('iso2c', 'iso3c', 'country', 'date')) |>
  select(-iso2c, -country) |>
  rename("year" = "date") |>
  pivot_longer(cols = 3:16, names_to = 'variable', values_to = 'value') |>
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

# Load SDG 4 data from input folder
# ed1 <- read_dta("input/EYS_data_update_2025 2.dta") %>%
#   filter(wbcode %in% aggs)  %>%
#   rename(iso3c = wbcode,
#          value = eys_mf_fill) %>%
#   mutate(variable = "HCI_EYRS") %>%
#   select(iso3c, year, variable, value)

# Load SDG 12 data from input folder
d12 <- read_excel("input/fossil_fuel_subsidy_global.xlsx", sheet = 1) %>%
  rename(value = `Fossil fuel subsidy (% of GDP)`,
         iso3c = region_code) %>%
  mutate(variable = "FF.SUB.GDP.ZS") %>%
  select(iso3c, year, value, variable)

# Load SDG 14 data from UNSDG website
# d14 <- jsonlite::fromJSON(paste('https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=EN_MAR_CHLDEV&pageSize=10000',sep=""), flatten = TRUE)$data %>%
#   as_tibble() %>%
#   filter(str_detect(geoAreaName, "World")) %>%
#   select(series, timePeriodStart, value) %>%
#   mutate(iso3c = "WLD") %>%
#   rename(year = timePeriodStart,
#          variable = series)

# # Load and process data for AI chapter
# d16 <- read.csv("input/chatgpt.csv") %>%
#   rename(iso3c = iso3,
#          value = traffic_per_internet_user) %>%
#   filter(iso3c %in% aggs) %>%
#   mutate(variable = "GPT.USE") %>%
#   select(iso3c, year, variable, value)

# Load and process data for ID ownership chapter
# d18 <- read.csv("input/ID_OWN.csv") %>%
#   filter(SEX == "_T",
#          AGE == "_T",
#          URBANISATION == "_T",
#          COMP_BREAKDOWN_1 == "_T",
#          COMP_BREAKDOWN_2 == "_T",
#          COMP_BREAKDOWN_3 == "_T",
#          REF_AREA %in% aggs) %>%
#   rename(iso3c = REF_AREA,
#          year = TIME_PERIOD,
#          value = OBS_VALUE) %>%
#   mutate(variable = "ID_OWN") %>%
#   select(iso3c, year, variable, value)

values_agg <- rbind(data_agg, d1, d12)

values_agg <- values_agg |>
  merge(meta, by.x = "variable", 
                by.y = "indicator", 
                all.x = T) |>
  mutate(value = round(value, rounding)) |>
  select(variable, iso3c, year, value, indicator_sdg) |>
  mutate(year = as.numeric(year)) |>
  filter(year > 2009)
  
write.csv(values_agg, "output/values_agg_sheet.csv", row.names = FALSE, na = '')