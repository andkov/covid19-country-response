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
# path_save <- paste0("./data-unshared/derived/ocdc-",Sys.Date(),".csv")
ds_covid <- readr::read_csv(config$path_input_covid)
ds_covid %>% glimpse()


# OECD
file_path <- list.files(config$path_oecd_clean,full.names = T,recursive = T,pattern = ".rds$")
dto <- list()
for(i in seq_along(file_path)){
  file_name <- basename(file_path[i]) %>% stringr::str_replace(".rds","")
  dto[[file_name]] <- readr::read_rds(file_path[i])
}

str(dto,max.level = 1)




# ---- bli -----------------------------
d_covid <- ds_covid %>% compute_epi_timeline()
d_covid %>% glimpse()

ds_bli <- dto$better_life_index %>%

ds_bli %>% glimpse()

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph -------------------------------------------------------------

g <- ds_covid %>%
  compute_epi_timeline() %>%
  # dplyr::filter(!country_code %in% c("USA","ITA","FRA","ESP","GBR") ) %>%
  # ggplot(aes(x = epi_timeline, y = log(n_deaths)))  +
  ggplot(aes(x = epi_timeline, y = n_deaths))  +
  geom_line(aes(group = country_code)) +
  theme_minimal()+
  # facet_wrap(~n_tile)+
  # geom_smooth(aes(x = epi_timeline, y = n_deaths, group = 1), inherit.aes=F, method = "loess", color = "gray70") +
  labs(
    # title = paste0(var_name," - ", unit_name)
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

