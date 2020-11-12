# Source
# https://covid19.who.int/table
# must update local repository before recreating the analysis-ready data file

# Population source WHO uses for shiny App
# https://population.un.org/wpp/Download/Standard/Population/


rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.
# Ideally, no real operations are performed.


# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified:
# http://r-pkgs.had.co.nz/namespace.html#search-path
library(tidyverse)

# ---- declare-globals ---------------------------------------------------------
config <- config::get()
path_source_who <- "https://covid19.who.int/WHO-COVID-19-global-data.csv"
path_source_pop <- paste0(
"https://population.un.org/wpp/Download/Files/"
,"1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv"
)

column_names <- c(
  "date"          = "Date_reported"
  ,"iso2"          = "Country_code"
  ,"country_label" = "Country"
  ,"who_region"    = "WHO_region"
  ,"n_cases"       = "New_cases"
  ,"n_cases_cum"   = "Cumulative_cases"
  ,"n_deaths"      = "New_deaths"
  ,"n_deaths_cum"  = "Cumulative_deaths"
)

# ---- load-data ---------------------------------------------------------------
# TODO: please process the data so it can be compared to other soruces
# Please add the population

ds_who <- read_csv(path_source_who)

ds_world_pop <-  read_csv(path_source_pop)

# For adding 3-letter codes (and potentially more)
ds_geo <- readr::read_csv("./data-public/metadata/world-geography.csv")

# ---- tweak-data -------------------

ds_who1 <- ds_who %>% rename(all_of(column_names)) %>%
  mutate(
    across(date, ~ymd(.))
  )

# need to multiply pop by 1000 to move the decimal over, not sure why
# file downloads with oddly placed decimal
ds_world_pop1 <- ds_world_pop %>%
  filter(Time == 2020, VarID == 2) %>%
  select(LocID,Location,Time,PopTotal) %>%
  mutate(across(PopTotal, ~.*1000))



ds_who_combined <- ds_who1 %>%
  left_join(ds_world_pop1, by = c("country_label" = "Location")) %>%
  select(-Time, -LocID) %>%
  rename(population = PopTotal) %>%
  left_join(
    ds_geo %>% distinct(iso2=country_code2, country_code)
    , by = "iso2"
  ) %>%
  select(date, iso2, country_code, everything())


# inspect
ds_who_combined %>% glimpse()


# stem to have the complete timeline
# dates     <- min(ds_who_combined$date):max(ds_who_combined$date)
# countries <- unique(ds_who_combined$country_code) %>% na.omit()
# ds_dates   <- tibble::as_tibble(
#   expand.grid(dates, countries, stringsAsFactors = F)
# )
# names(ds_dates) <- c("date", "country_code")
# ds_dates <- ds_dates %>%
#   dplyr::mutate(
#     date = lubridate::as_date(date)
#   )
# ds_dates %>% glimpse()
# ds_who_combined %>% glimpse()
#
# d <- ds_who_combined %>%
#   dplyr::left_join(ds_dates,by = c("date","country_code") ) %>%
#   tidyr::fill(population) %>%
#   dplyr::arrange(country_code, date)


# ---- save-to-disk ------------------------------------------------------------
ds_who_combined %>% readr::write_csv(config$path_input_who)








