#' ---
#' author: Kyle Belanger
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#'
#' ---

# data set for presidential county data 2020 retrieved from
# https://www.kaggle.com/unanimad/us-election-2020


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

ds0 <- read_csv("./data-unshared/raw/vote/president_county_candidate_2020.csv")



#' # Tweak Data
# ---- tweak-data --------------------------------------------------------------

ds_county <- ds0 %>% filter(party %in% c("DEM", "REP")) %>%
  select(-won, -candidate) %>%
  pivot_wider(
    names_from     = party
    ,values_from   = total_votes
    , names_prefix = "votes_"
    ) %>%
  janitor::clean_names() %>%
  mutate(
    winner = if_else(
      votes_rep > votes_dem
      ,"Republican"
      ,"Democrat"
      )
    )



ds_state <- ds_county %>%
  select(-winner) %>%
  group_by(state) %>%
  summarise(
    votes_dem = sum(votes_dem)
    ,votes_rep = sum(votes_rep)
    ) %>%
  mutate(
    winner = if_else(
      votes_rep > votes_dem
      ,"Republican"
      ,"Democrat"
    )
  )


#' # Save Data
# ---- save-data ---------------------------------------------------------------

ds_county %>% write_rds(
  "./data-public/derived/us-2020-county-pres-results.rds"
  ,compress = "gz"
  )

ds_state %>% write_rds(
  "./data-public/derived/us-2020-state-pres-results.rds"
  ,compress = "gz"
)














