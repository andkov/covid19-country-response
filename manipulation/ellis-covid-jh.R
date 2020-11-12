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

path_timeseries_cases_global <- "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
path_timeseries_deaths_global <- "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"


path_source_pop <- paste0(
  "https://population.un.org/wpp/Download/Files/"
  ,"1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv"
)

# ---- load-data ---------------------------------------------------------------
ds_world_pop <-  read_csv(path_source_pop)


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

ds_cases_global <- readr::read_csv(path_timeseries_cases_global)
ds_deaths_global <- readr::read_csv(path_timeseries_deaths_global)

ds_cases[1:13] %>% glimpse()
ds_deaths[1:13] %>% glimpse()

ds_cases_global[1:13] %>% glimpse()
ds_deaths_global[1:13] %>% glimpse()


# ---- tweak-data ------------
ds_world_pop1 <- ds_world_pop %>%
  filter(Time == 2020, VarID == 2) %>%
  select(LocID,Location,Time,PopTotal) %>%
  mutate(across(PopTotal, ~.*1000))
ds_world_pop1 %>% glimpse()

# ---- tweak-data-daily -----------------------
ds_daily <- ls_daily %>% bind_rows(.id = "date") %>% janitor::clean_names()
ds_daily %>% glimpse()


us_pop <- ds_deaths %>% janitor::clean_names() %>% select(1:12)
us_pop %>%  filter(country_region == "US") %>% glimpse()
ds_uspop <- us_pop %>%
  group_by(province_state) %>%
  summarize(
    population = sum(population, na.rm = T)
  ) %>%
  ungroup()
# add state population
ds_daily <- ds_daily %>%
  left_join(ds_uspop,by = "province_state")


# ---- tweak-data-timeseries-us -------------------

(stem_cases <- names(ds_cases)[1:11])
(stem_deaths <- names(ds_deaths)[1:12])


long_cases <- setdiff(names(ds_cases), stem_cases)
long_deaths <- setdiff(names(ds_deaths), stem_deaths)

ds_cases_long <- ds_cases %>%
  tidyr::pivot_longer(all_of(long_cases), names_to = "date", values_to = "n_cases") %>%
  dplyr::mutate(
    date = lubridate::mdy(date)
    ,across(FIPS, as.character)
    ,across(FIPS, ~if_else(str_length(.) < 5, paste0("0",.),.))
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
# ds_timeseries %>% distinct(province_state, country_region) %>% View()

# --- tweak-data-timeseries-global ---------------
(stem_cases_global <- names(ds_cases_global)[1:4])
(stem_deaths_global <- names(ds_deaths_global)[1:4])

long_cases_global <- setdiff(names(ds_cases_global), stem_cases_global)
long_deaths_global <- setdiff(names(ds_deaths_global), stem_deaths_global)


ds_cases_global_long <- ds_cases_global %>%
  tidyr::pivot_longer(all_of(long_cases_global), names_to = "date", values_to = "n_cases") %>%
  dplyr::mutate(
    date = lubridate::mdy(date)
  )

ds_cases_global_long %>% glimpse()


ds_deaths_global_long <- ds_deaths_global %>%
  tidyr::pivot_longer(all_of(long_deaths_global), names_to = "date", values_to = "n_deaths") %>%
  dplyr::mutate(
    date = lubridate::mdy(date)
  )
ds_deaths_global_long %>% glimpse()

ds_timeseries_global <-
  left_join(ds_cases_global_long, ds_deaths_global_long) %>%
  janitor::clean_names()

ds_timeseries_global %>% glimpse()

ds_timeseries_global %>% filter(country_region == "United Kingdom")

# The population adds the country_region level, but not province_state
d <- ds_timeseries_global %>%
  left_join(
    ds_world_pop1 %>% select(Location, population = PopTotal)
    , by = c("country_region" = "Location")) %>%


# d <- ds_timeseries_global %>%
#   left_join(
#     ds_timeseries %>% distinct(date, province_state, country_region, long, lat)
#   )
# ds_timeseries_global %>% distinct(province_state, country_region) %>% View()

# ----- global-timeseries ------------
# TODO: please create a `ds_timeseries_global` that would include US and list population for each country
# Note: the `./manipulation/ellis-covid.R` get the population number, or add new

# Please confirm to the names of the variables
# `n_cases`, `n_deaths`, `n_deaths_cum`, `n_cases_cum`, `country_code`, `country_label`

# ---- save-to-disk --------------------
ds_daily %>% readr::write_csv(config$path_input_jh_daily)
ds_timeseries %>% readr::write_csv(config$path_input_jh_usts)
# ds_daily %>% readr::write_csv("./data-unshared/derived/js_daily.rds")



















