# Source
# https://github.com/OxCGRT/covid-policy-tracker


rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.


# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(utils)
library(httr)
library(magrittr)
library(dplyr)

# ---- declare-globals ---------------------------------------------------------
config <- config::get()
path_file <- "../covid-policy-tracker/data/OxCGRT_latest.csv"
# path_file <- "../covid-policy-tracker/data/OxCGRT_latest_withoutnotes.csv"
path_xlsx <- "../covid-policy-tracker/data/timeseries/OxCGRT_timeseries_all.xlsx"

# ---- load-data ---------------------------------------------------------------
ds_covid <- readr::read_csv(config$path_input_covid)
ds_covid %>% glimpse()


ds_cgrt <- readr::read_csv(path_file)
ds_cgrt %>% glimpse()

# Excel source is in wide format, one tab per indicator
# sheet_names <- readxl::excel_sheets(path_xlsx)
# dto <- list()
# for(sheet_i in sheet_names){
#   # i <- sheet_names[1]
#   dto[[sheet_i]] <- readxl::read_xlsx(path_xlsx, sheet = sheet_i)
# }


# sources can be joined by the three letter country code
# ---- tweak-data -----------------------
cgrt_names <- names(ds_cgrt)
cgrt_names <- stringr::str_replace_all(cgrt_names, " ", "_")
cgrt_names <- stringr::str_replace_all(cgrt_names, "/", "_or_")
names(ds_cgrt) <- cgrt_names
ds_cgrt %>% glimpse()
ds_cgrt <- ds_cgrt %>%
  dplyr::mutate(
    Date = lubridate::ymd(Date)
  )

d <- ds_cgrt %>% filter(CountryCode == "AFG") %>% select(Date, CountryCode, ConfirmedCases, ConfirmedDeaths)
# some useful columns from the ECDC  covid source
ds_cgrt <- ds_cgrt %>%
  dplyr::rename(
    country_name = CountryName,
    country_code = CountryCode,
    date = Date
  )

ds_cgrt %>% glimpse()

ds_cgrt %>% readr::write_rds("./data-unshared/derived/OxCGRT.rds")



