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
ggplot2::theme_set(ggplot2::theme_bw())

# ---- declare-functions ---------------------------
compute_epi_timeline <- function(d, n_deaths_first_day = 1) { #}, d_country ){
  # browser()
  # d <- ds_cgrt %>%  filter(country_code %in% c("ITA","FRA") ) %>%
  # select(country_code, date, n_cases, n_deaths)
  #
  d_out <- d %>%
    # dplyr::filter(country_code %in% unique(d_country$id)) %>%
    dplyr::group_by(country_code) %>%
    dplyr::mutate(
      # this solution might be vulnerable to cases where some intermediate dates are missed
      n_deaths_cum  = cumsum(tidyr::replace_na(n_deaths,0))
      ,n_cases_cum  = cumsum(tidyr::replace_na(n_cases,0))
      ,cutoff_death = n_deaths_cum >= 1
      ,cutoff_case  = n_cases_cum >= 1
      ,days_since_1death = cumsum(tidyr::replace_na(cutoff_death,0))
      ,days_since_1case  = cumsum(tidyr::replace_na(cutoff_case,0))
    ) %>%
    dplyr::ungroup() %>%
    # dplyr::filter(epi_timeline > 0) %>%
    dplyr::mutate(
      days_since_exodus = date - lubridate::date("2020-01-13")

    ) %>%

    select(-cutoff_death, - cutoff_case)
  return(d_out)
}

# ---- load-data -------------------------------------------------------------
# list of focal countries in OECD database
ds_country <-
  readr::read_csv(
    config$path_country
  ) %>%
  dplyr::filter(desired)


# ECDC
# path_save <- paste0("./data-unshared/derived/ocdc-",Sys.Date(),".csv")
ds_covid <- readr::read_csv(config$path_input_covid)
# ds_covid %>% glimpse()


# OECD
file_path <- list.files(config$path_oecd_clean,full.names = T,recursive = T,pattern = ".rds$")
dto <- list()
for(i in seq_along(file_path)){
  file_name <- basename(file_path[i]) %>% stringr::str_replace(".rds","")
  dto[[file_name]] <- readr::read_rds(file_path[i])
}
# str(dto,max.level = 1)
ls_health_resources <- dto$health_resources
# ls_health_resources %>% str(1)
ds_hr <- ls_health_resources$data_agg
# ds_hr %>% glimpse()

# OxCGRT
ds_cgrt <- readr::read_rds("./data-unshared/derived/OxCGRT.rds")
# ds_cgrt %>% glimpse()
# n_distinct(ds_cgrt$country_name)
# ---- inspect-data ----------------------

# ---- tweak-data-1 ---------------
ds0 <- ds_cgrt %>%
  dplyr::mutate(
    n_deaths = tidyr::replace_na(n_deaths, 0)
    ,n_cases = tidyr::replace_na(n_cases, 0)
  ) %>%
  # select(country_code, country_name, date, ConfirmedCases, ConfirmedDeaths,n_cases, n_deaths, StringencyIndex) %>%
  # select(country_code, date, ConfirmedCases, ConfirmedDeaths,n_cases, n_deaths ) %>%
  # filter(country_code == "AFG") %>%
  compute_epi_timeline() %>%
  group_by(country_code) %>%
  mutate(
    n_deaths_cum_per_1m = n_deaths_cum/n_population_2018*1000000
    ,n_cases_cum_per_1m = n_cases_cum/n_population_2018*1000000

  ) %>%
  ungroup() %>%
  select(
    country_code, date, days_since_1death, days_since_1case, days_since_exodus,
    n_cases, n_deaths, n_deaths_cum, ConfirmedDeaths,n_cases_cum, ConfirmedCases,
    geo_id, country_name, continent,
    dplyr::everything()
  )





# n_distinct(ds0$country_code)
# test the logic
# d_out <- ds0 %>%  filter(country_code == "ITA") %>%
#   select(
#     country_code, date, days_since_1death, days_since_1case, days_since_exodus,
#     n_cases, n_deaths, n_deaths_cum, n_cases_cum,
#     geo_id, country_name,
#     StringencyIndex,
#     H1_Public_information_campaigns,  H2_Testing_policy, H3_Contact_tracing,
#     H4_Emergency_investment_in_healthcare, H5_Investment_in_vaccines
#   )

# ---- health-resources -----------------------------
ds1 <- ds0 %>%
  # filter(country_code %in% ds_country$id) %>%
  filter(country_code == "ITA") %>%
  dplyr::left_join(
    ds_hr
    ,by = c("country_code" = "location")
  )


# ----- q1 -----------------
# What the trend response to COVID-10 by each country?

d1 <- ds0 %>%
  filter(country_code %in% ds_country$id)
g1 <- d1 %>%
  ggplot(aes(x = days_since_exodus, y = StringencyIndex))+
  geom_line()+
  geom_point(data = d1 %>% filter(days_since_1case == 1), size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
  geom_point(data = d1 %>% filter(days_since_1death == 1), size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
  facet_wrap(~country_name)+
  labs(
    title = "Timeline of OECD countries' respones to COVID-19 as measured by the Stringency Index"
    ,y = "Stringency Index", x = "Days since first case outside of China (Jan 13, 2020)"
    , caption = "First dot = 1st confired case, Second dot = 1st confirmed death"
  )+
  geom_vline(xintercept = 60)
g1


# ----- q2 -----------------

d2 <- ds0 %>%
  filter(country_code %in% ds_country$id)
g2 <- d2 %>%
  filter(country_code %in% ds_country$id) %>%
  # filter(country_code == "ITA") %>%
  ggplot(aes(x = days_since_exodus, y = StringencyIndex, group = country_name))+
  geom_line(aes(color = continent), alpha = .4)+
  geom_point(data = d2 %>% filter(days_since_1case == 1), size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
    geom_point(data = d2 %>% filter(days_since_1death == 1), size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
  labs(
    title = "Timeline of OECD countries' respones to COVID-19 as measured by the Stringency Index"
    ,y = "Stringency Index", x = "Days since first case outside of China (Jan 13, 2020)"
    , caption = "First dot = 1st confired case, Second dot = 1st confirmed death"
  )
# g2 <- plotly::ggplotly(g2)
g2

# ---- q3 -------

d1 <- ds0 %>%
  # filter(country_code == "ITA") %>%
  # filter(country_code %in% ds_country$id) %>%
  filter(days_since_1case %in% c(30, 60) ) %>%
  select(country_code, days_since_1case, n_deaths_cum) %>%
  rename(
    temp_label = days_since_1case
  ) %>%
  mutate(
    temp_label = paste0("n_deaths_cum_",temp_label,"_days_since_1case")

  ) %>%
  tidyr::pivot_wider(
    names_from = temp_label, values_from = n_deaths_cum
  )

d2 <- ds0 %>%
  # filter(country_code == "ITA") %>%
  # filter(country_code %in% ds_country$id) %>%
  filter(days_since_1death %in% c(30, 60) ) %>%
  select(country_code, days_since_1death, n_deaths_cum) %>%
  rename(
    temp_label = days_since_1death
  ) %>%
  mutate(
    temp_label = paste0("n_deaths_cum_",temp_label,"_days_since_1death")

  ) %>%
  tidyr::pivot_wider(
    names_from = temp_label, values_from = n_deaths_cum
  )

d3 <- ds0 %>%
  # filter(country_code == "ITA") %>%
  filter(days_since_1case %in% c(1,30, 60) ) %>%
  select(country_code, days_since_1case, StringencyIndex) %>%
  rename(
    temp_label = days_since_1case
  ) %>%
  mutate(
    temp_label = paste0("StringencyIndex_",temp_label,"_days_since_1case")

  ) %>%
  tidyr::pivot_wider(
    names_from = temp_label, values_from = StringencyIndex
  )
# d_out <- dplyr::left_join(d1,d2) %>% dplyr::left_join(d3) %>%
#   filter(country_code %in% ds_country$id)
# d_out <- d1

d_out <- d1 %>%
  filter(country_code %in% ds_country$id) %>%
  # dplyr::left_join(
  #   ds0 %>% distinct(country_code, n_population_2018)
  # ) %>%
  # dplyr::mutate(
  #   n_deaths_cum_30_days_since_1case_per1m =n_deaths_cum_30_days_since_1case/1000000
  #   ,n_deaths_cum_60_days_since_1case_per1m =n_deaths_cum_60_days_since_1case /1000000
  # ) %>%
  dplyr::left_join(ds_hr, by = c("country_code" = "location"))
d_out %>% glimpse(30)
inds <- d_out %>% pull(indicator) %>% unique()
length(inds)
g1 <- d_out %>%
  # filter(IND %in% inds[1:C20x]) %>%
  filter(indicator %in% inds[21:41]) %>%
  ggplot(aes(x=value, y = n_deaths_cum_30_days_since_1case, label = country_code))+
  # ggplot(aes(x=value, y = n_deaths_per_1m, label = country_code))+
  # ggplot(aes(x=value, y = n_deaths, label = country_code))+
  # ggplot(aes(x=value, y = n_cases_per_1m, label = country_code))+
  # ggplot(aes(x=value, y = n_cases, label = country_code))+
  geom_point(shape = 21, fill = NA, alpha = 1, color = "salmon", size = 2)+
  geom_text()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~indicator, scales = "free")+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, color = "salmon", alpha = .6
    , vjust = 1
  )
g1

# ---- publish ---------------------------------------
path_report <- "./analysis/response-stringency-1/response-stringency-1.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)


