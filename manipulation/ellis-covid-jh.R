# Source
# https://github.com/CSSEGISandData/COVID-19
# must update local repository before recreating the analysis-ready data file

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
library(stringr)

# ---- declare-globals ---------------------------------------------------------
config <- config::get()
folder_daily_reports <- "../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports_us/"

# ---- load-data ---------------------------------------------------------------
daily_reports <- list.files(folder_daily_reports,pattern = "*.csv", full.names = T)
ls_daily <- list()
for(i in seq_along(daily_reports)){
# for(i in 1:10){
  # i <- 1
  i_name <- daily_reports[i] %>% basename() %>% str_remove(".csv")
  i_name <- i_name %>%  strptime("%m-%d-%Y") %>% lubridate::ymd()
  ls_daily[[as.character(i_name)]] <- readr::read_csv(daily_reports[i])

}

ds_daily <- ls_daily %>% bind_rows(.id = "date") %>% janitor::clean_names()

ds_daily %>% glimpse()

# ---- save-to-disk --------------------
# ds_daily %>% readr::write_csv(config$path_input_jh_daily)
ds_daily %>% readr::write_rds("./data-unshared/derived/js_daily.rds")



















