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
# ---- load-data ------------------------------------------------
# list of countries in the focus
ds_country <-
  readr::read_csv(
    config$path_country
  ) %>%
  dplyr::filter(desired)

# COVID
ds_covid <- readr::read_csv(config$path_input_covid)
ds_covid %>% glimpse()

# ---- inspect-data ------------------------------------------

# ---- define-functions --------------------------------------
# computes cumulative number of deaths since the first death
compute_epi_timeline <- function(d, n_deaths_first_day = 1) { #}, d_country ){
  # browser()
  # d_country <-
  #   readr::read_csv(
  #     # config$path_country
  #     "data-public/metadata/oecd/country.csv"
  #   ) %>%
  #   dplyr::filter(desired)

  d_out <- d %>%
    # dplyr::select(country_code, date, n_deaths) %>%
    # dplyr::filter(country_code %in% unique(d_country$id)) %>%
    dplyr::group_by(country_code) %>%
    dplyr::mutate(
      # this solution might be vulnerable to cases where some intermediate dates are missed
      n_deaths_cum = cumsum(n_deaths)
      ,cutoff = n_deaths_cum > n_deaths_first_day
      ,epi_timeline = cumsum(cutoff)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(epi_timeline > 0)
  return(d_out)
}
# d_covid <- ds_covid %>%
#   compute_epi_timeline(n_deaths_first_day = 1)
#


# ---- tweak-data -----------------------------------------------
# compute the epidemiological trajectory for each country
ds_covid <- ds_covid %>%
  dplyr::filter(
    country != c(
      "Cases_on_an_international_conveyance_Japan"
    )
  )
ds <- ds_covid %>% compute_epi_timeline(n_deaths_first_day = 0)
ds %>% glimpse()
# ---- basic-questions ----------------------
d <- ds_covid %>%
  compute_epi_timeline(n_deaths_first_day = 0) %>%
  dplyr::group_by(country) %>%
  dplyr::summarize(
    days_since_first_death = max(epi_timeline, na.rm =T)
    ,total_deaths = sum(n_deaths, na.rm = T)
    ,n_pop = unique(n_population_2018)
    ,deaths_per_1m = total_deaths/n_pop*1000000
  ) %>%
  dplyr::select(-n_pop) %>%
  dplyr::arrange(desc(deaths_per_1m))

g1 <- d %>%
  dplyr::filter(country != "San_Marino") %>%
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
  geom_point(shape = 21, )
g1 <- plotly::ggplotly(g1)
g1
# ---- trajectory-1 -------------
g <- d_covid %>%
  # dplyr::filter(country_code == "CHN") %>%
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
gp <- plotly::ggplotly(g)
gp


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

