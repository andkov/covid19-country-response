#' ---
#' author: Kyle Belanger
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#'
#' ---

# data set for presidential county data retrieved from
# https://electionlab.mit.edu/data


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

ds0 <- read_csv("./data-unshared/raw/vote/countypres_2000-2016.csv")



#' # Tweak Data
# ---- tweak-data -------------------------------------------------------------


ds1 <- ds0 %>% filter(year == 2016, party %in% c("democrat", "republican")) %>%
  select(year,state,county,FIPS,party,candidatevotes) %>%
  pivot_wider(
    names_from    = party
    ,names_prefix = "votes_"
    ,values_from  = candidatevotes
    ) %>%
  mutate(
    county_winner_2016 = if_else(
      votes_republican > votes_democrat
      ,"Republican"
      ,"Democrat"
      )
    ,across(FIPS, as.character)
    ,across(FIPS, ~if_else(str_length(.) < 5, paste0("0",.),.))
  )














