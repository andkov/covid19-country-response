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
# to help look for relevant variables and measures in this chaptert
# d_meta <- ls_input_health$health_resources %>% get_var_unit_lookup()
# d_meta <- ls_input_health$health_status %>% get_var_unit_lookup()

var_name <- "HOPITBED"
unit_name     <- "RTOINPNB"

prep_data_trajectory <- function(ls_oecd, df_covid, n_deaths_first_day = 1, var_name, unit_name){
  # browser()

  d_covid <- compute_epi_timeline(df_covid, n_deaths_first_day = n_deaths_first_day)
  d_oecd  <- ls_oecd %>% compute_rank(var_name = var_name, unit_name = unit_name)
  d_out <-   dplyr::left_join(
    d_covid, d_oecd, by = c("country_code" = "COU")
  )
  return(d_out)
}
# how to use
d_measure <- prep_data_trajectory(
  ls_oecd = ls_input_health$health_resources
  ,df_covid = ds_covid
  ,n_deaths_first_day = 1
  ,var_name = var_name
  ,unit_name = unit_name
)

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph -------------------------------------------------------------
d_measure %>% glimpse()
var_label <- d_measure %>%  dplyr::filter(!is.na(var_label)) %>% dplyr::pull(var_label) %>% unique(na.rm=T)
unit_label <- d_measure %>%  dplyr::filter(!is.na(unit_label)) %>% dplyr::pull(unit_label) %>% unique(na.rm=T)
g <- d_measure %>%
  # dplyr::filter(epi_timeline <=30) %>%
  # ggplot(aes(x = epi_timeline, y = n_deaths, color = rank_percentile)) +
  ggplot(aes(x = epi_timeline, y = n_deaths, color = factor(n_tile))) +
  geom_line(aes(group = country_code))+
  theme_minimal()+
  facet_wrap(~n_tile)+
  labs(
    title = paste0(var_label," - ", unit_label)
  )
g <- plotly::ggplotly(g)
g
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

