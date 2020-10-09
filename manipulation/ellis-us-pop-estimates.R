#' ---
#' author: Andriy Koval
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#'
#' ---

#+ include = FALSE
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console


# ---- knitr-opts --------------------------------------------------------------
#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
knitr::opts_knit$set(root.dir = "../")

# ---- load-sources ------------------------------------------------------------

#' # Load Packages
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

#' Declare Globals
# ---- declare-globals ---------------------------------------------------------



#' # Load Data
# ---- load-data ---------------------------------------------------------------
# Source: https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/
# Dictionary : https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.pdf
ds0 <- read_csv(
  "./data-unshared/raw/us-census/co-est2019-alldata.csv"
  ) %>% janitor::clean_names()

ds_county_fips <- readr::read_csv("./data-public/metadata/us/county_fips.csv")
ds_states <- readr::read_csv("./data-public/metadata/us/state-abb.csv")

#' # Tweak Data
# ---- tweak-data -------------------------------------------------------------
ds_county_fips <- ds_county_fips %>%
  mutate(
    fips = stringr::str_sub(county_fips, 1L,2L)
  ) %>%
  left_join(ds_states, by = c("state_name" = "state", "state_abb" = "abb"))

# rm(ds_states)

ds1 <- ds0 %>%
  # select(sumlev, region, division, state, county, stname, ctyname, popestimate2019) %>%
  mutate(
    fips = paste0(state,county)
  ) %>%
  mutate(
    region = fct_recode(as.character(region),
                        "NorthEast" = "1"
                        ,"MidWest" = "2"
                        ,"South" = "3"
                        ,"West" = "4"
                        )
    ,division = fct_recode(as.character(division),
                           "New England" = "1"
                           ,"Middle Atlantic" = "2"
                           ,"East North Central" = "3"
                           ,"West North Central" = "4"
                           ,"South Atlantic" = "5"
                           ,"East South Central" = "6"
                           ,"West South Central" = "7"
                           ,"Mountain" = "8"
                           ,"Pacific" = "9"


                           )
  )

ds1 %>% distinct(region, division)
#' # Join
# ---- join-data ---------------------------------------------------------------




#' # Save Data
# ---- save-data ---------------------------------------------------------------

ds1 %>% write_rds("./data-public/derived/us-pop-estimate.rds"
                  ,compress = "gz")

# ds1 %>% write_csv(
#   gzfile("./data-public/derived/us-pop-estimate-2010-2019.csv"))


