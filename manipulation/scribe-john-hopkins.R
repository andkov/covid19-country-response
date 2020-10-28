# This script reads two files: patient event table + location map.
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(forcats)
library(stringr)
library(lubridate)
library(knitr) # dynamic documents
library(rmarkdown) # dynamic
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
library(plotly)
# library(TabularManifest) # exploratory data analysis, see https://github.com/Melinae/TabularManifest
requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables

#---- load-sources ------------------------------------------------------------
config <- config::get()
source("./scripts/common-functions.R")        # reporting functions and quick views
# source("./scripts/graphing/graph-presets.R") # font and color conventions
# source("./scripts/graphing/graph-support.R") # font and color conventions

# ---- declare-globals --------------------
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)

# ---- declare-functions ---------------------------

# ---- load-data -------------------------------------------------------------
# Source: John Hopkins
# Data description: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
# USA daily state reports (csse_covid_19_daily_reports_us)
# Produced by `./manipulation/ellis-covid-jh.R`
ds_daily <- readr::read_csv(config$path_input_jh_daily)# state level, no county level, but contains `people_tested` metric
# Time series summary (csse_covid_19_time_series)
# ds_daily %>% glimpse()
# Produced by `./manipulation/ellis-covid-jh.R`
ds_usts <- readr::read_csv(config$path_input_jh_usts) # timeseries

# Source: US Census
# Produced by: `./manipulation/ellis-us-pop-estimates.R`
# Total US population estimate
ds_us_pop <- readr::read_rds("./data-public/derived/us-pop-estimate.rds")

# Source: Harvard Datavers (presidential) + Kaiser Foundation (state parties)
# Produced by `./manipulation/ellis-us-election-results-2.R`
ds_vote <- readr::read_rds("./data-public/derived/us-2020-state-political-results.rds")
# Note: political leadership reflects the state of 2020

# ---- tweak-data --------------------

# To obtain state, division, region
ds_us_pop <- ds_us_pop %>%
  select(sumlev, region, division, state, county,fips, state_name = stname, county_name = ctyname, popestimate2016, popestimate2019)
# ds_us_pop %>% glimpse()

# For verification only, inherit population number from JH `time_series_covid19_deaths_US`
ds_us_pop_state <- ds_us_pop %>%
  filter(sumlev == "040") %>%
  select(-sumlev) %>%
  left_join(tibble::tibble(state_name = state.name, state_abb = state.abb)) %>%
  mutate(stabb = ifelse(state_name == "District of Columbia","DC",state_abb))
# ds_us_pop_state %>% glimpse() #

varnames_crouise <- c("Diamond Princess","Grand Princess" )
varnames_territories <- c(
  "American Samoa"
  ,"Guam"
  ,"Northern Mariana Islands"
  ,"Puerto Rico"
  ,"Virgin Islands"
)

ds_usts <- ds_usts %>%
  left_join(
    ds_us_pop %>% distinct(region, division,state_name)
    , by  = c("province_state" = "state_name")
  ) %>%
  # don't use rates here, but save for later if needed
  mutate(
    incident_rate = n_cases/population*100000
    ,mortality_rate = n_deaths/population*100000
  ) %>%
  # to simplify by removing small populations of cruiseship and territories
  dplyr::mutate(
    region = dplyr::case_when(
      (province_state %in% varnames_crouise) ~ "Cruiseship",
      (province_state %in% varnames_territories) ~ "Territories",
      TRUE ~ as.character(region)
      ) %>% as_factor(),
    division = dplyr::case_when(
      province_state %in% varnames_crouise ~ "Cruiseship",
      province_state %in% varnames_territories ~ "Territories",
      TRUE ~ as.character(division)
    ) %>% as_factor()
  )

# ds_usts %>% glimpse()
# Gives casese and deaths at county level. NO TESTING data.

# create a state-level version of usts for the purposes of joining with ds_daily dataframe
group_by_vars <- c("date", "province_state", "division", "region", "country_region")
summarize_vars <- c("n_cases","n_deaths","population")
ds_usts_state <- ds_usts %>%
  group_by(.dots = group_by_vars) %>%
  summarize_at(.vars = summarize_vars, .funs = sum, na.rm = T) %>%
  ungroup()
# add testing data from the ds_daily
# ds_daily %>% glimpse()
# ds_usts_state %>% glimpse()
ds_jh_state <- ds_usts_state %>%
  left_join(
    ds_daily %>% select(date,province_state, people_tested)
    , by = c("date","province_state")
  ) %>%
  filter(!region %in% c("Territories", "Cruiseship") ) %>%
  mutate(
    n_tested = people_tested
    ,state   = factor(province_state)
    ,country = factor(country_region)
  ) %>%
  select(
    date, state, division, region, country,
    n_cases, n_deaths, n_tested,
    population
  )
ds_jh_state %>% glimpse()

# ds_covid_vote <- ds_jh_state %>%
#   left_join(
#     ds_vote %>% select(-c("state_po","state_fips")), by = c("state"= "province_state")
#   )
# ds_covid_vote %>% glimpse()


# ----- save-to-disk  -----------------------

ds_jh_state %>% readr::write_rds("./data-unshared/derived/john-hopkins-state.rds", compress="gz")
# ds_covid_vote %>% readr::write_rds("./data-unshared/derived/covid-vote.rds", compress = "gz")


# ----- explain-verify --------------
ds_jh_state %>% glimpse()

# Population is correct
selected_state  <- "Florida"
ds_jh_state %>% filter(state == selected_state) %>% pull(population) %>% unique()

# 50 states + DC
ds_jh_state %>%
  distinct(state,population) %>%
  group_by(state) %>% summarize(n_residents = sum(population, rm.na=T)) %>%
  neat()

# All measures are cumulative
d <- ds_jh_state %>%
  filter(state == "Florida") %>%
  tidyr::pivot_longer(cols = c("n_cases","n_deaths","n_tested"),
                      names_to = "measure", values_to = "value")
d %>% ggplot(aes(x=date, y = value))+
  geom_line()+
  facet_wrap(~measure, scales = "free")









