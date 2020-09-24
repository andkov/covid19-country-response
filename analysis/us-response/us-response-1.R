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
# )

print_plotly_lines <- function(d, measure = "confirmed", default_region = "Florida", ...){
  # d <- ds_daily
  g1 <- d %>%
    plotly::highlight_key(~ province_state) %>%
    ggplot(aes_string(x = "date", y = measure, group = "province_state"))+
    geom_line( alpha = .3) +
    scale_y_continuous(labels = scales::comma)+
    labs(
      x = "Date", ...

    )
  # g1
  g1p <-
    plotly::ggplotly(g1) %>%                    # make into a plotly object
    plotly::highlight(                         # add highlight functionality
      on             = "plotly_click"          # or "plotly_hover"
      ,dynamic       = TRUE                    # adds color option
      ,selectize     = TRUE                    # select what to highlight
      ,defaultValues = default_region          # highlights in the beginning
    ) %>%
    plotly::layout(margin = list(l = 0, r = 0, b = 80, t = 30, pad = 0))
  # plotly::layout(margin = margings_for_plotly)
  g1p

}




# ds_daily %>% print_plotly_lines("confirmed", y  = "Confirmed Cases", title = "XXX")
# ds_daily %>% print_plotly_lines("active")

# ---- load-data -------------------------------------------------------------
# reference table for geographic units
# ds_geo <- readr::read_csv("./data-public/metadata/world-geography.csv")
ds_geo <- readr::read_csv("../COVID-19/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv") %>% janitor::clean_names()
# ds_geo %>% glimpse()
ds_covid <- readr::read_csv(config$path_input_covid)
# ds_covid %>% glimpse()
ds_cgrt <-  readr::read_rds(config$path_input_cgrt)
# ds_cgrt %>% glimpse()
ds_daily <- readr::read_csv(config$path_input_jh_daily)
# ds_daily %>% glimpse()

# to keep it manageble during exploration
# ds_cgrt <- ds_cgrt %>%  select(country_code, date, StringencyIndex )
# ds_cgrt %>% glimpse()

# n_distinct(ds_cgrt$country_code)
# ds_covid$country_code %>% unique() %>% length()
# ds_cgrt$country_code %>% unique() %>% length()

# ---- tweak-data --------------------

ds_cgrt <- ds_cgrt %>%
  mutate(
    region_code = ifelse(is.na(region_code) & country_code == "USA" , "USA", region_code)
    ,region_name = ifelse(is.na(region_name)& country_code == "USA", "USA", region_name)
  ) %>%
  rename(
    province_state = region_name
  ) %>%
  filter(
    country_code == "USA"
  )


# #
# ds_daily <- ds_daily %>%
#   mutate(
#     confirmed_100k = (confirmed / population) * 100000
#     deaths_100k = (confirmed / population) * 100000
#     recovered_100k = (confirmed / population) * 100000
#     active_100k = (confirmed / population) * 100000
#     people_tested_100k = (confirmed / population) * 100000
#     people_hospitalized_100k = (confirmed / population) * 100000
#   )

# ---- inspect-data ----------------------

# ds_daily %>% distinct(country_region)

# ---- declare-functions2 -----------------
# d <- ds_daily %>%
#   filter(province_state == "Florida")

# g <- d %>%
#   ggplot(aes(x = date, y = confirmed ))+
#   geom_line()
# g
#
# g + aes(y = deaths)
# g + aes(y = recovered) # nothing
# g + aes(y = active)
# g + aes(y = incident_rate) # transformed active
# g + aes(y = people_tested)
# g + aes(y = people_hospitalized)
# g + aes(y = mortality_rate)
# g + aes(y = testing_rate)
# g + aes(y = hospitalization_rate)
#
# g <- ds_daily %>%
#   # filter(province_state %in% c("Florida","New York")) %>%
#   ggplot(aes(x = date, y = confirmed ))+
#   geom_line(aes(group =province_state ))+
#   geom_smooth()
# g
#

# confirmed - incident_rate
# deaths - mortality_rate
# people_tested - testing_rate
# people_hospitalized - hospitalization_rate
#
# ds_daily %>% glimpse()
# print_state_facets <- function(d, measure){
#   d <- ds_daily
#   # measure <- "confirmed"
#   measure <- "incident_rate"
#
#   g1 <- d %>%
#     ggplot(aes_string(x = "date", y = measure, group = "province_state"))+
#     geom_line( alpha = .9) +
#     scale_y_continuous(labels = scales::comma)+
#     facet_wrap(~province_state,ncol = 7)+
#     labs( x = "Date")
#   g1
#
# }




# ---- confirmed -------------------------
ds_daily %>% print_plotly_lines("confirmed", y = "Confirmed Cases", title = "Timeline of confirmed cases by state")


ds_daily %>% print_plotly_lines("incident_rate", y = "Incident Rate", title = "Timeline of incident rate by state")

# ---- deaths -------------------------
ds_daily %>% print_plotly_lines("deaths", y = "Deaths", title = "Timeline of deaths by state")


ds_daily %>% print_plotly_lines("mortality_rate", y = "Mortality Rate", title = "Timeline of mortality rate by state")

# ---- tested -------------------------
ds_daily %>% print_plotly_lines("people_tested", y = "People Tested", title = "Timeline of people tested by state")


ds_daily %>% print_plotly_lines("testing_rate", y = "Testing Rate", title = "Timeline of testing rate by state")

# ---- hospitalized -------------------------
ds_daily %>% print_plotly_lines("people_hospitalized", y = "People Hospitalized", title = "Timeline of hospitalizations by state")


ds_daily %>% print_plotly_lines("hospitalization_rate", y = "Hospitalization Rate", title = "Timeline of hospitalization rate by state")


# ---- stringency ----------------------
ds_cgrt %>% print_plotly_lines("stringency_index", y = "Stringency Index", title = "Timeline of stringency index by state", default_region = "USA")

# ---- government ----------------------
ds_cgrt %>% print_plotly_lines("government_response_index", y = "Government Response Index", title = "Timeline of Gov Response index by state", default_region = "Florida")

# ---- containment ----------------------
ds_cgrt %>% print_plotly_lines("containment_health_index ", y = "Containment Health Index", title = "Timeline of Containment Health index by state", default_region = "USA")

# ---- economy ----------------------
ds_cgrt %>% print_plotly_lines("economic_support_index ", y = "Economic Support Index", title = "Timeline of Economic Support index by state", default_region = "USA")




# ---- publish ---------------------------------------
path_report <- "./analysis/us-response/us-response-1.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)






