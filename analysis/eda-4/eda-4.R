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
# source("./scripts/graphing/graph-presets.R") # font and color conventions
# source("./scripts/graphing/graph-support.R") # font and color conventions

# ---- declare-globals --------------------
ggplot2::theme_set(
    ggplot2::theme_bw()+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)
# ---- declare-functions ---------------------------
compute_epi_timeline <- function(d, n_deaths_first_day = 1) { #}, d_country ){
  # browser()
  # d <- ds_cgrt %>%
  #   # filter(country_code %in% c("ITA","FRA") ) %>%
  #   filter(country_code %in% c("AFG") ) %>%
  # select(country_code, date, n_cases, n_deaths)
  #
  d_out <- d %>%
    # dplyr::filter(country_code %in% unique(d_country$id)) %>%
    dplyr::group_by(country_code) %>%
    dplyr::mutate(
      # this solution might be vulnerable to cases where some intermediate dates are missed
      n_deaths_cum         = cumsum(tidyr::replace_na(n_deaths,0))
      ,n_cases_cum         = cumsum(tidyr::replace_na(n_cases,0))
      ,n_deaths_cum_per_1m = n_deaths_cum/n_population_2018*1000000
      ,n_cases_cum_per_1m  = n_cases_cum/ n_population_2018*1000000

      ,cutoff_death        = n_deaths_cum >= 1
      ,cutoff_case         = n_cases_cum >= 1
      ,days_since_1death   = cumsum(tidyr::replace_na(cutoff_death,0))
      ,days_since_1case    = cumsum(tidyr::replace_na(cutoff_case,0))
      ,date_of_1death      = lubridate::as_date(ifelse(days_since_1death==1,date, NA))
      ,date_of_1case       = lubridate::as_date(ifelse(days_since_1case==1,date, NA))
      ,date_of_1death      = min(date_of_1death, na.rm =T)
      ,date_of_1case       = min(date_of_1case, na.rm =T)
      ,days_since_1death   = (date - date_of_1death) %>% as.integer()
      ,days_since_1case    = (date - date_of_1case) %>% as.integer()

    ) %>%
    dplyr::ungroup() %>%
    # dplyr::filter(epi_timeline > 0) %>%
    dplyr::mutate(
      days_since_exodus    = as.integer(date - lubridate::date("2020-01-13")) # first case outside of china
      ,days_since_pandemic = as.integer(date - lubridate::date("2020-03-11")) # WHO declares pandemic
    ) %>%
    select(-cutoff_death, - cutoff_case, -date_of_1death, -date_of_1case)
  return(d_out)
}

# for testing the function:
# d_out <- ds0 %>%  filter(country_code == "ITA") %>%
#     select(
#       country_code, date,n_cases, n_deaths, ConfirmedDeaths, ConfirmedCases
#     ) %>%
#   compute_epi_timeline()


# ---- load-data -------------------------------------------------------------
# reference table for geographic units (see ./manipulation/ellis-geography.R)
ds_geo <- readr::read_csv("./data-public/metadata/world-geography.csv")
# ds_geo %>% glimpse()

# COVID spread and mortality (see ./manipulation/ellis-covid.R)
ds_covid <- readr::read_csv(config$path_input_covid)
# ds_covid %>% glimpse()

# OxCGRT - COVID Government Response Tracker (see ./manipulation/ellis-cgrt.R)
ds_cgrt <- readr::read_rds("./data-unshared/derived/OxCGRT.rds")
# to keep it manageble during exploration
ds_cgrt <- ds_cgrt %>%  select(country_code, date, StringencyIndex,GovernmentResponseIndex,ContainmentHealthIndex,EconomicSupportIndex )
# ds_cgrt %>% glimpse()

ls_performance <- readr::read_rds("./data-unshared/derived/performance-metrics.rds")
ds_performance_long <- ls_performance$long
ds_performance_wide <- ls_performance$wide
ds_performance_wide %>% glimpse()

# OECD - Organization for Economic Cooperation and Development database - https://stats.oecd.org/
# see ./manipulation/ellis-oecd.R
file_path <- list.files(config$path_oecd_clean,full.names = T,recursive = T,pattern = ".rds$")
dto <- list()
for(i in seq_along(file_path)){
  file_name <- basename(file_path[i]) %>% stringr::str_replace(".rds","")
  dto[[file_name]] <- readr::read_rds(file_path[i])
}
# str(dto,max.level = 1)
ls_health_resources <- dto$health_resources # one facet
# ls_health_resources %>% str(1)
ds_hr <- ls_health_resources$data_agg # aggregated dataset from that facet
# ds_hr %>% glimpse()

# n_distinct(ds_cgrt$country_code)
# ds_covid$country_code %>% unique() %>% length()


# ---- inspect-data ----------------------
# date_i <- lubridate::as_date("2020-03-20")

# ds_covid %>% filter(date == date_i) %>% group_by(country_code) %>% count() %>% arrange(desc(n))
# ds_cgrt %>% filter(date == date_i) %>% group_by(country_code) %>% count() %>% arrange(desc(n))
# ds0 %>% filter(date == date_i) %>% group_by(country_code) %>% count() %>% arrange(desc(n))

# ---- tweak-data-1 ---------------
ds0 <- ds_covid %>%
  compute_epi_timeline() %>%
  dplyr::left_join(
    ds_cgrt
    ,by = c("date", "country_code")
  ) %>%
  # filter(country_code %in% c("TUR", "ARM")) %>%
  # filter(date == date_i) %>%
  dplyr::left_join(
    ds_geo %>% select(-country_name, -country_number),
    by = c("country_code" )
  ) %>%
  filter(
    !is.na(country_label)
  )

ds0 %>% glimpse()
#



# ----- ----------------------
featured_metrics <- c(
  "n_deaths_30days_since_1death_per1m" = "Deaths 30 days since 1st Death (per 1m)"
  # ,"n_deaths_30days_since_1case_per1m" = "Deaths 30 days since 1st Case (per 1m)"
  ,"n_cases_30days_since_1death_per1m" = "Cases 30 days since 1st Death (per 1m"
  # ,"n_cases_30days_since_1case_per1m"  = "Cases 30 days since 1st Case (per 1m)"
  ,"n_cases_100days_since_exodus_per_1m" = "Cases 100 days since Exodus (per 1m)"
  ,"n_deaths_100days_since_exodus_per_1m" = "Deaths 100 days since Exodus (per 1m)"
  ,"days_from_exodus_to_1case" = "Days from Exodus to 1st Case"
  ,"days_from_exodus_to_1death" = "Days from Exodus to 1st Death"
)
ds_performance_long %>% glimpse()
ds_performance_wide %>% glimpse()

# ---- performance-metrics-1 ----------------------
# create a exploration scatterplot matrix

ds_performance_wide %>%
  filter(oecd) %>%
  ggplot(aes(x =n_deaths_30days_since_1death_per1m, y = n_deaths_100days_since_exodus_per_1m  ))+
  geom_text(aes(label = country_code))+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, vjust = 3
  )



d <- ds_performance_wide %>%
  filter(cou)
  select(c(names(featured_metrics)), country_code)
d %>% glimpse()

GGally::ggpairs(
  ds_performance_wide %>% filter(oecd)
  , names(featured_metrics)
)


# ---- -----------------


# ----- family ---------------------------
# ls <- readr::read_rds(paste0(config$path_oecd_clean,"family.rds"))
# ds <- ls$data_agg
# ds %>% glimpse()
# ds$location %>% unique() %>% length()

# ----- sc ---------------------------
ls <- readr::read_rds(paste0(config$path_oecd_clean,"serving_citizens.rds"))
ds_citserv <- ls$data_agg %>%
  # filter(country_code == "ITA")
  select(location, indicator, value) %>%
  mutate(
    indicator = as.character(indicator)
  ) %>%
  dplyr::rename(
    metric = indicator, country_code = location
  ) %>%
  mutate(
    metric = snakecase::to_snake_case(metric)
  )

ds_citserv %>% glimpse()
# ds_citserv$metric %>% unique()

ds1_long <- ds_performance_long %>%
  select(country_code, metric, value) %>%
  # filter(country_code == "ITA") %>%
  bind_rows(ds_citserv) %>%
  dplyr::left_join(
    ds_geo, by = "country_code"
  )

ds1_long$metric %>% unique()

ds1_wide <- ds1_long %>%
  tidyr::pivot_wider(names_from = "metric", values_from = "value" )
ds1_wide %>% glimpse()
list(
  "wide" = ds1_wide, "long" = ds1_long
) %>%
  readr::write_rds("./analysis/shiny-since-metric/data.rds")

# ---- 1 ------------------

metric_predictor <- "n_deaths_100days_since_exodus_per_1m"
metric_outcome <- c( c(ds_citserv %>% pull(metric) %>% unique()))[1:20]

g1 <- ds1_long %>%
  filter(metric %in% c(metric_outcome, metric_predictor) ) %>%
  ggplot(aes_string(x = metric_predictor, y = "value")) +
  geom_text(aes(label = country_code))+
  facet_wrap(~metric)
g1


# ---- publish ---------------------------------------
path_report <- "./analysis/eda-2/eda-2.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)


