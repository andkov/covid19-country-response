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

ds0 <- read_csv(
  "../external/US_County_Level_Election_Results_08-16/2016_US_County_Level_Presidential_Results.csv"
  )

#' # Tweak Data
# ---- tweak-data -------------------------------------------------------------

ds1 <- ds0 %>%
  select(-X1) %>%
  mutate(
    county_fips = as.character(combined_fips)
    # ,fip4 = nchar(combined_fips)==4L
    ,county_fips = ifelse(nchar(combined_fips)==4L,paste0("0",county_fips), county_fips)
  )
ds1 %>% glimpse()



#' # Save Data
# ---- save-data ---------------------------------------------------------------

ds1 %>% write_rds("./data-unshared/derived/us-2016-election-results.rds"
                  ,compress = "gz")

# ds1 %>% write_csv(
#   gzfile("./data-public/derived/us-pop-estimate-2010-2019.csv"))


