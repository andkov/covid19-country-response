# This script reads two files: patient event table + location map.
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(knitr) # dynamic documents
library(rmarkdown) # dynamic
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
# library(TabularManifest) # exploratory data analysis, see https://github.com/Melinae/TabularManifest
requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables
# requireNamespace("plyr", quietly=TRUE)
# requireNamespace("reshape2", quietly=TRUE) #For converting wide to long
# requireNamespace("mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.

# ---- load-sources ------------------------------------------------------------
config <- config::get()
source("./scripts/common-functions.R")        # reporting functions and quick views
# source("./scripts/graphing/graph-presets.R") # font and color conventions
# source("./scripts/graphing/graph-support.R") # font and color conventions

# ---- declare-globals --------------------
ggplot2::theme_set(
  ggplot2::theme_bw()+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)
# ---- declare-functions ---------------------------
compute_epi_timeline <- function(d, n_deaths_first_day = 1) { #}, d_country ){
  # browser()
  # d <- ds_cgrt %>%
  #   # filter(country_code %in% c("ITA","FRA") ) %>%
  #   filter(country_code %in% c("AFG") ) %>%
  # select(country_code, date, n_cases, n_deaths)
  #
  d_out <- d %>%
    # dplyr::filter(country_code %in% unique(d_country$id)) %>%
    dplyr::group_by(country_code) %>%
    dplyr::mutate(
      # this solution might be vulnerable to cases where some intermediate dates are missed
      n_deaths_cum         = cumsum(tidyr::replace_na(n_deaths,0))
      ,n_cases_cum         = cumsum(tidyr::replace_na(n_cases,0))
      ,n_deaths_cum_per_1m = n_deaths_cum/n_population_2018*1000000
      ,n_cases_cum_per_1m  = n_cases_cum/ n_population_2018*1000000

      ,cutoff_death        = n_deaths_cum >= 1
      ,cutoff_case         = n_cases_cum >= 1
      ,days_since_1death   = cumsum(tidyr::replace_na(cutoff_death,0))
      ,days_since_1case    = cumsum(tidyr::replace_na(cutoff_case,0))
      ,date_of_1death      = lubridate::as_date(ifelse(days_since_1death==1,date, NA))
      ,date_of_1case       = lubridate::as_date(ifelse(days_since_1case==1,date, NA))
      ,date_of_1death      = min(date_of_1death, na.rm =T)
      ,date_of_1case       = min(date_of_1case, na.rm =T)
      ,days_since_1death   = (date - date_of_1death) %>% as.integer()
      ,days_since_1case    = (date - date_of_1case) %>% as.integer()

    ) %>%
    dplyr::ungroup() %>%
    # dplyr::filter(epi_timeline > 0) %>%
    dplyr::mutate(
      days_since_exodus    = as.integer(date - lubridate::date("2020-01-13")) # first case outside of china
      ,days_since_pandemic = as.integer(date - lubridate::date("2020-03-11")) # WHO declares pandemic
    ) %>%
    select(-cutoff_death, - cutoff_case, -date_of_1death, -date_of_1case)
  return(d_out)
}

# for testing the function:
# d_out <- ds0 %>%  filter(country_code == "ITA") %>%
#     select(
#       country_code, date,n_cases, n_deaths, ConfirmedDeaths, ConfirmedCases
#     ) %>%
#   compute_epi_timeline()


# ---- load-data -------------------------------------------------------------
# reference table for geographic units (see ./manipulation/ellis-geography.R)
ds_geo <- readr::read_csv("./data-public/metadata/world-geography.csv")
# ds_geo %>% glimpse()

# COVID spread and mortality (see ./manipulation/ellis-covid.R)
ds_covid <- readr::read_csv(config$path_input_covid)
# ds_covid %>% glimpse()

# OxCGRT - COVID Government Response Tracker (see ./manipulation/ellis-cgrt.R)
ds_cgrt <- readr::read_rds("./data-unshared/derived/OxCGRT.rds")
# to keep it manageble during exploration
ds_cgrt <- ds_cgrt %>%  select(country_code, date, StringencyIndex,GovernmentResponseIndex,ContainmentHealthIndex,EconomicSupportIndex )
# ds_cgrt %>% glimpse()

# ---- inspect-data ----------------------
# date_i <- lubridate::as_date("2020-03-20")

# ds_covid %>% filter(date == date_i) %>% group_by(country_code) %>% count() %>% arrange(desc(n))
# ds_cgrt %>% filter(date == date_i) %>% group_by(country_code) %>% count() %>% arrange(desc(n))
# ds0 %>% filter(date == date_i) %>% group_by(country_code) %>% count() %>% arrange(desc(n))

# ---- tweak-data-1 ---------------
ds0 <- ds_covid %>%
  compute_epi_timeline() %>%
  dplyr::left_join(
    ds_cgrt
    ,by = c("date", "country_code")
  ) %>%
  # filter(country_code %in% c("TUR", "ARM")) %>%
  # filter(date == date_i) %>%
  dplyr::left_join(
    ds_geo %>% select(-country_name, -country_number),
    by = c("country_code" )
  ) %>%
  filter(
    !is.na(country_label)
  )

ds0 %>% glimpse()
# ---- covid-metric-1 -----------------------
# d1 <- ds0 %>%
d_out <- ds0 %>%
  filter(country_code == "ITA") %>%
  select(country_code, date,n_cases_cum, n_deaths_cum, days_since_1case, days_since_1death)

# ds0 <- ds0 %>% filter(country_code == "BTN")


d0 <- ds_geo %>% distinct(country_code) %>% na.omit()

# Deaths 30 days after 1st death
d1 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1death == 30) %>%
  dplyr::select(country_code, n_deaths_cum, n_deaths_cum_per_1m) %>%
  dplyr::rename(n_deaths_30days_since_1death       = n_deaths_cum) %>%
  dplyr::rename(n_deaths_30days_since_1death_per1m = n_deaths_cum_per_1m) %>%
  ungroup()
d1 %>% glimpse()
# Deaths 60 days after 1st death
d2 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1death == 60) %>%
  dplyr::select(country_code, n_deaths_cum,n_deaths_cum_per_1m) %>%
  dplyr::rename(n_deaths_60days_since_1death       = n_deaths_cum) %>%
  dplyr::rename(n_deaths_60days_since_1death_per1m = n_deaths_cum_per_1m)%>%
  ungroup()
d2 %>% glimpse()
# Cases 30 days after 1st case
d3 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1case == 30) %>%
  dplyr::select(country_code, n_cases_cum, n_cases_cum_per_1m) %>%
  dplyr::rename(n_cases_30days_since_1case       = n_cases_cum) %>%
  dplyr::rename(n_cases_30days_since_1case_per1m = n_cases_cum_per_1m)%>%
  ungroup()
d3 %>% glimpse()

# Cases 60 days after 1st case
d4 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1case == 60) %>%
  dplyr::select(country_code, n_cases_cum, n_cases_cum_per_1m) %>%
  dplyr::rename(n_cases_60days_since_1case       = n_cases_cum) %>%
  dplyr::rename(n_cases_60days_since_1case_per1m = n_cases_cum_per_1m) %>%
  ungroup()
d4 %>% glimpse()

# Deaths 30 days after 1st case
d5 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1case == 30) %>%
  dplyr::select(country_code, n_deaths_cum, n_deaths_cum_per_1m) %>%
  dplyr::rename(n_deaths_30days_since_1case       = n_deaths_cum) %>%
  dplyr::rename(n_deaths_30days_since_1case_per1m = n_deaths_cum_per_1m) %>%
  ungroup()
d5 %>% glimpse()

# Deaths 60 days after 1st case
d6 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1case == 60) %>%
  dplyr::select(country_code, n_deaths_cum, n_deaths_cum_per_1m) %>%
  dplyr::rename(n_deaths_60days_since_1case       = n_deaths_cum) %>%
  dplyr::rename(n_deaths_60days_since_1case_per1m = n_deaths_cum_per_1m)%>%
  ungroup()
d6 %>% glimpse()

# Cases 30 days after 1st death
d7 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1death == 30) %>%
  dplyr::select(country_code, n_cases_cum, n_cases_cum_per_1m) %>%
  dplyr::rename(n_cases_30days_since_1death = n_cases_cum) %>%
  dplyr::rename(n_cases_30days_since_1death_per1m = n_cases_cum_per_1m)%>%
  ungroup()
d7 %>% glimpse()
# Cases 60 days after 1st case
d8 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1death == 60) %>%
  dplyr::select(country_code, n_cases_cum, n_cases_cum_per_1m) %>%
  dplyr::rename(n_cases_60days_since_1death = n_cases_cum) %>%
  dplyr::rename(n_cases_60days_since_1death_per1m = n_cases_cum_per_1m) %>%
  ungroup()
d8 %>% glimpse()

# Deaths 100 days since exodus
d9 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_exodus == 100) %>%
  dplyr::select(country_code, n_deaths_cum, n_deaths_cum_per_1m) %>%
  dplyr::rename(n_deaths_100days_since_exodus       = n_deaths_cum) %>%
  dplyr::rename(n_deaths_100days_since_exodus_per_1m = n_deaths_cum_per_1m)%>%
  ungroup()
d9 %>% glimpse()

# Cases 100 days since exodus
d10 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_exodus == 100) %>%
  dplyr::select(country_code, n_cases_cum, n_cases_cum_per_1m) %>%
  dplyr::rename(n_cases_100days_since_exodus = n_cases_cum) %>%
  dplyr::rename(n_cases_100days_since_exodus_per_1m = n_cases_cum_per_1m) %>%
  ungroup()
d10 %>% glimpse()

# how many days have passed between the first detected case and the first death?
d11 <- ds0 %>%
  # filter(oecd) %>%
  filter(days_since_1case == 0) %>%
  mutate(
    date_1case                = date
    ,days_from_exodus_to_1case = days_since_exodus
    ,days_from_1case_to_1death = -1*days_since_1death
    ,stringency_1case          = StringencyIndex,
  ) %>%
  distinct(country_code, date_1case,  days_from_exodus_to_1case, days_from_1case_to_1death, stringency_1case  )
d11 %>% glimpse()

# what was the response stringency on the date of first death?
d12 <- ds0 %>%
  # filter(oecd) %>%
  filter(days_since_1death == 0) %>%
  dplyr::rename(
    date_1death       = date
    ,days_from_exodus_to_1death = days_since_exodus
    ,stringency_1death = StringencyIndex
  ) %>%
  distinct(country_code, date_1death, days_from_exodus_to_1death, stringency_1death)
d12 %>% glimpse()

# ds0 %>% filter(country_ == "LVA")
ds_scince_metric <- list(
  d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12
) %>% Reduce( function(a,b) dplyr::left_join(a,b), . )
ds_scince_metric %>% glimpse()

longer_names <- setdiff(names(ds_scince_metric),c("country_code","date_1case","date_1death"))
ds_scince_metric_long <- ds_scince_metric %>%
  select(-date_1case, -date_1death) %>%
  tidyr::pivot_longer(cols = longer_names, names_to = "metric", values_to = "value")
# ds_scince_metric_long %>% glimpse()

ds_scince_metric <- ds_scince_metric %>%
  dplyr::left_join(
    ds_geo, by = c("country_code")
  ) %>%
  dplyr::filter(!is.na(country_code))

ds_scince_metric_long <- ds_scince_metric_long %>%
  dplyr::left_join(
    ds_geo, by = c("country_code")
  ) %>%
  dplyr::filter(!is.na(country_code))

# ds_scince_metric_long %>% glimpse()
# ds_scince_metric %>% glimpse()

# ----- save-to-disk -------------------
# ds_scince_metric %>% neat_DT()
ls_scince_metric <- list(
  "wide" = ds_scince_metric, "long" = ds_scince_metric_long
)
ls_scince_metric %>%  readr::write_rds("./data-unshared/derived/performance-metrics.rds")

