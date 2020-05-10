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
ggplot2::theme_set(ggplot2::theme_bw())

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
      ,cutoff_death        = n_deaths_cum >= 1
      ,cutoff_case         = n_cases_cum >= 1
      ,days_since_1death   = cumsum(tidyr::replace_na(cutoff_death,0))
      ,days_since_1case    = cumsum(tidyr::replace_na(cutoff_case,0))
      ,date_of_1death      = lubridate::as_date(ifelse(days_since_1death==1,date, NA))
      ,date_of_1case       = lubridate::as_date(ifelse(days_since_1case==1,date, NA))
      ,date_of_1death      = min(date_of_1death, na.rm =T)
      ,date_of_1case       = min(date_of_1case, na.rm =T)
      ,days_since_1death   = date - date_of_1death
      ,days_since_1case    = date - date_of_1case
      ,n_deaths_cum_per_1m = n_deaths_cum/n_population_2018*1000000
      ,n_cases_cum_per_1m  = n_cases_cum/ n_population_2018*1000000
    ) %>%
    dplyr::ungroup() %>%
    # dplyr::filter(epi_timeline > 0) %>%
    dplyr::mutate(
      days_since_exodus = date - lubridate::date("2020-01-13") # first case outside of china
      ,days_since_pandemic = date - lubridate::date("2020-03-11") # WHO declares pandemic
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
# list of focal countries in OECD database
ds_country <-
  readr::read_csv(
    config$path_country
  ) %>%
  dplyr::filter(desired)
#
# # ECDC
# # path_save <- paste0("./data-unshared/derived/ocdc-",Sys.Date(),".csv")
ds_covid <- readr::read_csv(config$path_input_covid,)
# ds_covid %>% glimpse()

ds_country_codes <- readr::read_csv(config$path_country_codes)

# OECD
file_path <- list.files(config$path_oecd_clean,full.names = T,recursive = T,pattern = ".rds$")
dto <- list()
for(i in seq_along(file_path)){
  file_name <- basename(file_path[i]) %>% stringr::str_replace(".rds","")
  dto[[file_name]] <- readr::read_rds(file_path[i])
}
# str(dto,max.level = 1)
ls_health_resources <- dto$health_resources
# ls_health_resources %>% str(1)
ds_hr <- ls_health_resources$data_agg
# ds_hr %>% glimpse()

# OxCGRT
ds_cgrt <- readr::read_rds("./data-unshared/derived/OxCGRT.rds")
# ds_cgrt %>% glimpse()
# n_distinct(ds_cgrt$country_code)

# ---- inspect-data ----------------------

# ---- tweak-data-1 ---------------
ds0 <- ds_covid %>%
  compute_epi_timeline() %>%
  dplyr::left_join(
    ds_cgrt
    ,by = c("date", "country_code")
  ) %>%
  dplyr::left_join(
    ds_country_codes,
    by = c("country_code" = "country_code3")
  )

unique(ds_covid$country_code)
unique(ds0$country_code)
d_out <- ds0 %>% filter(country_code == "ITA")

# ---- performance-indicators -----------------------
# d1 <- ds0 %>%
d_out <- ds0 %>%
  filter(country_code == "ITA") %>%
  select(country_code, date,n_cases_cum, n_deaths_cum, days_since_1case, days_since_1death)

# Deaths 30 days after 1st death
d1 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1death == 30) %>%
  dplyr::select(country_code, n_deaths_cum, n_population_2018) %>%
  dplyr::rename(n_deaths_30days_since_1death = n_deaths_cum) %>%
  dplyr::mutate(n_deaths_30days_since_1death_per1m = n_deaths_30days_since_1death/n_population_2018*1000000 ) %>%
  dplyr::select(-n_population_2018)

# Deaths 60 days after 1st death
d2 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1death == 60) %>%
  dplyr::select(country_code, n_deaths_cum,n_population_2018) %>%
  dplyr::rename(n_deaths_60days_since_1death = n_deaths_cum) %>%
  dplyr::mutate(
    n_deaths_60days_since_1death_per1m = n_deaths_60days_since_1death/n_population_2018*1000000
  ) %>%
  dplyr::select(-n_population_2018)

# Cases 30 days after 1st case
d3 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1case == 30) %>%
  dplyr::select(country_code, n_cases_cum, n_population_2018) %>%
  dplyr::rename(n_cases_30days_since_1case = n_cases_cum) %>%
  dplyr::mutate(
    n_cases_30days_since_1case_per1m = n_cases_30days_since_1case/n_population_2018*1000000
  ) %>%
  dplyr::select(-n_population_2018)

# Cases 60 days after 1st case
d4 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1case == 60) %>%
  dplyr::select(country_code, n_cases_cum, n_population_2018) %>%
  dplyr::rename(n_cases_60days_since_1case = n_cases_cum) %>%
  dplyr::mutate(
    n_cases_60days_since_1case_per1m = n_cases_60days_since_1case/n_population_2018*1000000
  ) %>%
  dplyr::select(-n_population_2018)

# ds0 %>% filter(country_ == "LVA")
ds_response <- list(d1,d2,d3,d4) %>% Reduce(function(a,b) dplyr::full_join(a,b), .)
ds_response <- ds_response %>%
  dplyr::left_join(
    ds_covid %>% distinct(country_code, geo_id, country)
  ) %>%
  dplyr::filter(!is.na(country_code))

ds_response %>% neat_DT

# ---- publish ---------------------------------------
path_report <- "./analysis/response-stringency-1/response-stringency-1.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)


