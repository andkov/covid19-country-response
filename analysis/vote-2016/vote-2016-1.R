# This script reads two files: patient event table + location map.
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(forcats)
library(stringr)
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

# margings_for_plotly <- list(
#   l = 50,
#   r = 50,
#   b = 100,
#   t = 100,
#   pad = 4
# )\






# ds_daily %>% print_plotly_lines("confirmed", y  = "Confirmed Cases", title = "XXX")
# ds_daily %>% print_plotly_lines("active")
# ds_cgrt %>% print_plotly_lines("stringency_index",grouping = "region_code",  default_region = c("USA","GBR","IRL","CAN"))

# ---- load-data -------------------------------------------------------------
# reference table for geographic units

#  2016 election results
ds_election <- readr::read_rds("./data-unshared/derived/us-2016-election-results.rds")
# US population estimate of adults broken down by age and race groups
ds_us_pop_age_race <- readr::read_rds("./data-public/derived/us-pop-estimate-2010-2019.rds")
# Total US population estimate
ds_us_pop <- readr::read_rds("./data-public/derived/us-pop-estimate.rds")
# county-level covid data
ds_covid_nyt_county <- readr::read_csv("../covid-19-data/us-counties.csv")


# ---- tweak-data --------------------

ds_vote <- ds_election %>%
  select()

ds_vote %>% glimpse()

ds_us_pop <- ds_us_pop %>%
  select(sumlev, region, division, state, county,fips, state_name = stname, county_name = ctyname, popestimate2019)

ds_us_pop_state <- ds_us_pop %>%
  filter(sumlev == "040") %>%
  select(-sumlev) %>%
  left_join(tibble::tibble(state_name = state.name, state_abb = state.abb)) %>%
  mutate(stabb = ifelse(state_name == "District of Columbia","DC",state_abb))

ds_us_pop_county <- ds_us_pop %>%
  filter(sumlev == "050") %>%
  select(-sumlev) %>%
  left_join(tibble::tibble(state_name = state.name, state_abb = state.abb)) %>%
  mutate(
    county_name = stringr::str_remove_all(county_name," County")
  )
ds_us_pop_county %>% glimpse()

ds_covid <-
  ds_covid_nyt_county %>%
  filter(county != "Unknown") %>%
  rename(
    n_cases_cum   = cases
    ,n_deaths_cum = deaths
  ) %>%
  left_join( ds_us_pop_county %>% select(fips, region, division, n_population_2019 = popestimate2019 )) %>%
  dplyr::group_by(fips) %>%
  dplyr::mutate(
    # this solution might be vulnerable to cases where some intermediate dates are missed
     n_deaths_cum_per_1m = n_deaths_cum/n_population_2019*1000000
    ,n_cases_cum_per_1m  = n_cases_cum/ n_population_2019*1000000

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
  ungroup()

ds_covid %>% glimpse()

ds_covid_vote <-
  ds_covid %>%
  left_join(ds_election %>% select(fips = county_fips, votes_dem, votes_gop, total_votes,
                                   per_dem, per_gop))

ds_covid_vote %>% glimpse()

# ----- ---------------

d <- ds_covid_vote %>%
  group_by(fips) %>%
  mutate(
    max_cases = max(n_cases_cum, na.rm = T)
    ,max_deaths = max(n_deaths_cum, na.rm = T)
  ) %>%
  ungroup() %>%
  filter(date == lubridate::as_date("2020-10-01"))
g1 <- d %>%
  ggplot(aes(x = per_gop, y = max_cases))+
  geom_point(shape = 21, alpha = .3)
g1

d %>% ggplot()+
  geom_histogram(aes(x=per_gop), fill = "red", alpha =.5)+
  geom_histogram(aes(x=per_dem), fill = "blue", alpha =.5)+
  labs(title = "How many counties ")

# ------ ----------------------
ds_election %>% glimpse()
ds_covid_nyt_county %>% glimpse()




# ---- publish ---------------------------------------

path_report <- "./analysis/us-response/us-response-2.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)




