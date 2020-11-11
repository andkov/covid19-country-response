# Source
# https://covid19.who.int/table
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
path_source <- "https://covid19.who.int/WHO-COVID-19-global-data.csv"

# ---- load-data ---------------------------------------------------------------
# TODO: please process the data so it can be compared to other soruces
# Please add the population

# ---- tweak-data -------------------

# ---- save-to-disk --------------------
