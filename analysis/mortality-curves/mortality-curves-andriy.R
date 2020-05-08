# run ellis to get the data, disable after first use
# base:::source("./manipulation/ellis-covid.R")

rm(list=ls(all=TRUE)) #Clear the memory
cat("\f") # clear console when working in RStudio

# ---- load-packages --------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names.
library(knitr) # dynamic documents
library(rmarkdown) # dynamic
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables

# ---- load-sources ----------------------------------------------
config <- config::get()
#set default ggplot theme
ggplot2::theme_set(ggplot2::theme_bw())
base::source("./scripts/common-functions.R")
# ---- load-data ------------------------------------------------
# list of countries in the focus
ds_country <-
  readr::read_csv(
    config$path_country
  )
# %>%
  # dplyr::filter(desired)

# COVID
ds_covid <- readr::read_csv(config$path_input_covid)
ds_covid %>% glimpse()
ds_country %>% glimpse()
# ---- inspect-data ------------------------------------------

# ---- define-functions --------------------------------------

# ---- tweak-data -----------------------------------------------
# compute the epidemiological trajectory for each country
ds_covid <- ds_covid %>%
  dplyr::filter(
    country != c(
      "Cases_on_an_international_conveyance_Japan"
    )
  )

# create a generic data set for general use
ds <- ds_covid %>% compute_epi_timeline(n_deaths_first_day = 0)
ds %>% glimpse()

# ---- basic-questions ----------------------
# How do countries compare on total/relative death counts,
# and days of epidemic
d <- ds_covid %>%
  dplyr::filter(
    country_code %in% (ds_country %>% pull(id))
  ) %>%
  compute_epi_timeline(n_deaths_first_day = 0) %>%
  dplyr::group_by(country, country_code) %>%
  dplyr::summarize(
    days_since_first_death = max(epi_timeline, na.rm =T)
    ,total_deaths = sum(n_deaths, na.rm = T)
    ,n_pop = unique(n_population_2018)
    ,deaths_per_1m = total_deaths/n_pop*1000000
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-n_pop) %>%
  dplyr::arrange(desc(deaths_per_1m))
d %>% arrange(days_since_first_death) #%>%  neat_DT()

g1 <- d %>%
  # dplyr::filter(country != "San_Marino") %>%
  dplyr::filter(
    country_code %in% (ds_country %>% pull(id))
  ) %>%
  ggplot(aes(
    group = country
    , x = total_deaths
    # , x = deaths_per_1m
    # , x = days_since_first_death
    # , y = total_deaths
    , y = deaths_per_1m
    # , y = total_deaths
    , size = days_since_first_death
    # , size = deaths_per_1m
    ))+
  geom_point(shape = 21, alpha = .1)+
  geom_text(aes(label = country))
# g1 <- plotly::ggplotly(g1)
g1

# ---- trajectory-1 -------------
d <- ds_covid %>%
  dplyr::filter(
    country_code %in% (ds_country %>% pull(id))
  ) %>%
  compute_epi_timeline(n_deaths_first_day = 0) %>%
  dplyr::mutate(
    deaths_per_1m = n_deaths/n_population_2018*1000000
  )

g <- d %>%
  dplyr::filter(country_code %in% (ds_country %>% pull(id))) %>%
  compute_epi_timeline(n_deaths_first_day = 0) %>%
  # dplyr::filter(country_code == "CHN") %>%
  # dplyr::filter(!country_code %in% c("USA","ITA","FRA","ESP","GBR") ) %>%
  # ggplot(aes(x = epi_timeline, y = log(n_deaths)))  +
  # ggplot(aes(x = epi_timeline, y = n_deaths))  +
  # ggplot(aes(x = epi_timeline, y = n_deaths_cum))  +
  ggplot(aes(x = epi_timeline, y = deaths_per_1m))  +
  geom_line(aes(group = country_code)) +
  theme_minimal()+
  facet_wrap(~country, scales = "free_y")+
  # geom_smooth(aes(x = epi_timeline, y = n_deaths, group = 1), inherit.aes=F, method = "loess", color = "gray70") +
  labs(
    # title = paste0(var_name," - ", unit_name)
  )
# gp <- plotly::ggplotly(g)
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

