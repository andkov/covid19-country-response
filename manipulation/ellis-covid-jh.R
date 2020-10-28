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
path_timeseries_cases_us <- "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
path_timeseries_deaths_us <- "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
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
ds_cases <- readr::read_csv(path_timeseries_cases_us)
ds_deaths <- readr::read_csv(path_timeseries_deaths_us)

ds_cases %>% glimpse()
ds_deaths %>% glimpse()
# ---- tweak-data -------------------

(stem_cases <- names(ds_cases)[1:11])
(stem_deaths <- names(ds_deaths)[1:12])


long_cases <- setdiff(names(ds_cases), stem_cases)
long_deaths <- setdiff(names(ds_deaths), stem_deaths)

ds_cases_long <- ds_cases %>%
  tidyr::pivot_longer(all_of(long_cases), names_to = "date", values_to = "n_cases") %>%
  dplyr::mutate(
    date = lubridate::mdy(date)
  )

ds_cases_long %>% glimpse()


ds_deaths_long <- ds_deaths %>%
  tidyr::pivot_longer(all_of(long_deaths), names_to = "date", values_to = "n_deaths") %>%
  dplyr::mutate(
    date = lubridate::mdy(date)
    ,across(FIPS, as.character)
    ,across(FIPS, ~if_else(str_length(.) < 5, paste0("0",.),.))
  )
ds_deaths_long %>% glimpse()


ds_timeseries <-
  left_join(ds_deaths_long, ds_cases_long) %>%
  janitor::clean_names()

ds_timeseries %>% glimpse()


ds_daily <- ls_daily %>% bind_rows(.id = "date") %>% janitor::clean_names()
ds_daily %>% glimpse()


us_pop <- ds_deaths %>% janitor::clean_names() %>% select(1:12)
us_pop %>%  filter(country_region == "US") %>% glimpse()
ds_uspop <- us_pop %>%
  group_by(province_state) %>%
  summarize(
    population = sum(population, na.rm = T)
  )
# add state population
ds_daily <- ds_daily %>%
  left_join(ds_uspop,by = "province_state")




# ---- save-to-disk --------------------
ds_daily %>% readr::write_csv(config$path_input_jh_daily)
ds_timeseries %>% readr::write_csv(config$path_input_jh_usts)
# ds_daily %>% readr::write_csv("./data-unshared/derived/js_daily.rds")



















