# Source
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide



rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
config <- config::get()

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(utils)
library(httr)
library(magrittr)
library(dplyr)

# ---- declare-globals ---------------------------------------------------------
path_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
# ---- load-data ---------------------------------------------------------------

#download the dataset from the ECDC website to a local temporary file
# GET(url = path_url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
# ds_ocdc_raw <- read.csv(tf)
# path_save <- paste0("./data-unshared/derived/ocdc-",Sys.Date(),".csv")
# readr::write_csv(ds_ocdc_raw,path_save)
# # run above line once per update

ds_covid <- readr::read_csv(path_save)
ds_covid %>% glimpse()
# ---- tweak-data -----------------------
names(ds_covid) <- c("date", "day", "month", "year", "n_cases", "n_deaths", "country", "geo_id", "country_code","n_population_2018")
# ---- reconcile-countries -------------------
ds_country <-
  readr::read_csv(
    config$path_country
  ) %>%
  dplyr::filter(desired)

ds_country <- ds_country %>%
  dplyr::left_join(
    ds_covid %>% dplyr::distinct(country,country_code)
    ,by = c("id" = "country_code")
  )

# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------





#read the Dataset sheet into “R”. The dataset will be called "data".
ds <- read.csv(tf)
ds %>% glimpse()




ds <- ds %>%
  dplyr::mutate(
    date = lubridate::dmy(date)
  )
ds %>% glimpse()

ds %>% dplyr::distinct( country)

ds %>% dplyr::distinct(date) %>% arrange(date)
