# This script needs to run before publishing App.  Opens the local versions
# of needed data and places them into a folder in the app


rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio
# ---- load-packages -----------------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
# ---- load-sources ------------------------------------------------------------

# updates local data prior to creating app_data
source("./manipulation/ellis-covid-jh.R")
source("./manipulation/scribe-john-hopkins.R")


# ---- declare-globals ---------------------------------------------------------

dir_path <- "./"


# ---- load-data ---------------------------------------------------------------
# Produced by `./manipulation/scribe-john-hopkins.R`
ds_jh_state <- readr::read_rds(
  paste0(dir_path,"data-unshared/derived/john-hopkins-state.rds")
)
# Source: Harvard Datavers (presidential) + Kaiser Foundation (state parties)
# Produced by `./manipulation/ellis-us-election-results-2.R`
ds_vote <- readr::read_rds(
  paste0(dir_path,"data-public/derived/us-2020-state-political-results.rds")
)
# Note: political leadership reflects the state of 2020

ds_vote_pres_2020 <-  read_rds(
  paste0(dir_path, "data-public/derived/us-2020-state-pres-results.rds")
  )

# ---- merge-data --------------------------------------------------------------

ds_covid_vote <- ds_jh_state %>%
  left_join(
    ds_vote %>% select(-c("state_po","state_fips"))
    ,by = c("state"= "province_state")
  ) %>%
  left_join(ds_vote_pres_2020, by = "state") %>%
  mutate(state = factor(state))


# ---- save-data ---------------------------------------------------------------
# saves the combine data set into the app directory /data

ds_covid_vote %>% write_rds(
  "./analysis/shiny-covid-votes/data/app-data.rds"
  ,compress = "gz"
  )
