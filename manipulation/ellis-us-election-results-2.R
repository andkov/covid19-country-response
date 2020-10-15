#' ---
#' author: Kyle Belanger
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#'
#' ---

# data set for presidential data retrieved from
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX

# data set for state parties
# https://www.kff.org/other/state-indicator/state-political-parties


#+ include = FALSE
#These first few lines run only when the file is run in RStudio,
#!!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console


# ---- knitr-opts --------------------------------------------------------------
#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
knitr::opts_knit$set(root.dir = "../")

# ---- load-sources ------------------------------------------------------------

#' # Load Packages
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be
# qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

#' Declare Globals
# ---- declare-globals ---------------------------------------------------------

#' # Load Data
# ---- load-data ---------------------------------------------------------------

ds0_pres <- read_csv("./data-unshared/raw/vote/1976-2016-president.csv")

ds0_state <- read_csv(
  "./data-unshared/raw/vote/2020_state_political_parties.csv"
  ,skip = 2)

#' # Tweak Data
# ---- tweak-data -------------------------------------------------------------
ds0_pres %>% glimpse()
ds1_pres <- ds0_pres %>%
  filter(year == 2016 & party %in% c("republican", "democrat") & !is.na(candidate)) %>%
  #maryland listed twice, second time only shows 200 votes
  filter(candidatevotes > 300) %>%
  select( province_state = state, state_po, state_fips, party, candidatevotes, totalvotes) %>%
  pivot_wider(
    names_from = party
    ,names_prefix = "votes_"
    ,values_from = candidatevotes
    ) %>%
  mutate(
    winner_2016 = ifelse(votes_republican > votes_democrat, "republican", "democrat" )
  )

ds1_pres %>% glimpse()


ds1_state <- ds0_state %>%
  filter(
    Location != "United States" & !is.na(`Governor Political Affiliation`)
    ) %>%
  janitor::clean_names() %>%
  select(
    province_state = location
    ,governor_political_affiliation
    ,state_senate_majority_political_affiliation
    ,state_house_majority_political_affiliation
    ,state_attorney_general_political_affiliation
    )


ds2 <- ds1_pres %>%
  left_join(ds1_state, by = c("province_state"))


#' # Save Data
# ---- save-data ---------------------------------------------------------------

ds2 %>% write_rds("./data-public/derived/us-2020-state-political-results.rds"
                  ,compress = "gz")
