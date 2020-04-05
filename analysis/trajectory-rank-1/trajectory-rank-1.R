# This script reads two files: patient event table + location map.
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(knitr) # dynamic documents
library(rmarkdown) # dynamic
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
# library(TabularManifest) # exploratory data analysis, see https://github.com/Melinae/TabularManifest
requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables
# requireNamespace("plyr", quietly=TRUE)
# requireNamespace("reshape2", quietly=TRUE) #For converting wide to long
# requireNamespace("mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.

# ---- load-sources ------------------------------------------------------------
config <- config::get()
source("./scripts/common-functions.R")        # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions
source("./scripts/graphing/graph-support.R") # font and color conventions

# ---- load-data -------------------------------------------------------------
ds_country <-
  readr::read_csv(
    config$path_country
  ) %>%
  dplyr::filter(desired)

# COVID
path_save <- paste0("./data-unshared/derived/ocdc-",Sys.Date(),".csv")
ds_covid <- readr::read_csv(path_save)
ds_covid %>% glimpse()

# OEDC
path_folder_oecd_health <- "./data-unshared/raw/oecd/health/"
input_files_oecd_health <- list.files(path_folder_oecd_health, pattern = ".rds$",  full.names = T)
file_names <- gsub(".rds$","", basename(input_files_oecd_health))

ls_input_health <- list()
for(i in seq_along(input_files_oecd_health)){
  file_name_i <- gsub(".rds$","", basename(input_files_oecd_health[i]))
  ls_input_health[[file_name_i]] <- readr::read_rds(input_files_oecd_health[i])
}
# ---- inspect-data -------------------------------------------------------------

# ---- tweak-data --------------------------------------------------------------
d_covid <- ds_covid %>%
  # dplyr::select(country_code, date, n_deaths) %>%
  dplyr::filter(country_code %in% unique(ds_country$id)) %>%
  dplyr::group_by(country_code) %>%
  dplyr::mutate(
    # this solution might be vulnerable to cases where some intermediate dates are missed
    n_deaths_cum = cumsum(n_deaths)
    ,cutoff = n_deaths_cum > 0
    ,epi_timeline = cumsum(cutoff)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(epi_timeline > 0)
d_covid %>% print(n = nrow(.))

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph -------------------------------------------------------------
d_meta <- ls_input_health$health_resources %>% get_var_unit_lookup()

var_name <- "HOPITBED"
unit_name     <- "RTOINPNB"

d_measure <- ls_input_health$health_resources %>% compute_rank(var_name, unit_name)



# Sonata form report structure
# ---- dev-a-0 ---------------------------------
# ---- dev-a-1 ---------------------------------
# ---- dev-a-2 ---------------------------------
# ---- dev-a-3 ---------------------------------
# ---- dev-a-4 ---------------------------------
# ---- dev-a-5 ---------------------------------

# ---- dev-b-0 ---------------------------------
# ---- dev-b-1 ---------------------------------
# ---- dev-b-2 ---------------------------------
# ---- dev-b-3 ---------------------------------
# ---- dev-b-4 ---------------------------------
# ---- dev-b-5 ---------------------------------

# ---- recap-0 ---------------------------------
# ---- recap-1 ---------------------------------
# ---- recap-2 ---------------------------------
# ---- recap-3 ---------------------------------


# ---- publish ---------------------------------------
path_report_1 <- "./reports/*/report_1.Rmd"
path_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(path_report_1,path_report_2)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {

  rmarkdown::render(input = pathFile,
                    output_format=c(
                      # "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}

