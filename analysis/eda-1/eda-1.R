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
  ggplot2::theme_bw(
  )+
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
      ,cutoff_death        = n_deaths_cum >= 1
      ,cutoff_case         = n_cases_cum >= 1
      ,days_since_1death   = cumsum(tidyr::replace_na(cutoff_death,0))
      ,days_since_1case    = cumsum(tidyr::replace_na(cutoff_case,0))
      ,date_of_1death      = lubridate::as_date(ifelse(days_since_1death==1,date, NA))
      ,date_of_1case       = lubridate::as_date(ifelse(days_since_1case==1,date, NA))
      ,date_of_1death      = min(date_of_1death, na.rm =T)
      ,date_of_1case       = min(date_of_1case, na.rm =T)
      ,days_since_1death   = date - date_of_1death
      ,days_since_1case    = date - date_of_1case
      ,n_deaths_cum_per_1m = n_deaths_cum/n_population_2018*1000000
      ,n_cases_cum_per_1m  = n_cases_cum/ n_population_2018*1000000
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
margings_for_plotly <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)

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

# ---- inspect-data ----------------------

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

# ds0 %>% glimpse()

# ----- q3-response-all -----------------

# Why 75 days after exodus should be the starting point?
# 1. Most countries have peaked in their response
d1 <- ds0 %>% filter(oecd)
g1 <- ds0 %>%
  # filter(country_code == "ITA") %>%
  ggplot(aes(x = days_since_exodus, y = StringencyIndex, group = country_label, color = oecd))+
  geom_line( alpha = .2)+
  scale_color_manual(values = c("TRUE" = "darkorchid3", "FALSE" = "grey50"))+
  geom_point(data = d1 %>% filter(days_since_1case == 1), size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
  geom_point(data = d1 %>% filter(days_since_1death == 1), size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
  scale_x_continuous(breaks = seq(0,100, 25))+
  labs(
    title = "Timeline of countries' respones to COVID-19 as measured by the Stringency Index"
    ,y = "Stringency Index", x = "Days since first case outside of China (Jan 13, 2020)"
  )+
  geom_vline(xintercept = 58, linetype = "dotted")+
  geom_vline(xintercept = 75, linetype = "dashed")+
  geom_vline(xintercept = 100, linetype = "dashed", color = "red")
margings_for_plotly <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)
g1 <- plotly::ggplotly(g1)
g1 %>% plotly::layout(autosize = F, width = 900, height = 600, margin = margings_for_plotly)


# ----- q3-toll-all ----------------------
# 2. This is when the mortality curves starts going up
d2 <- ds0 %>% filter(oecd)
g2 <- ds0 %>%
  # filter(country_code %in% ds_country$id) %>%
  # filter(country_code == "ITA") %>%
  ggplot(aes(x = days_since_exodus, y = n_deaths_cum_per_1m, group = country_label, color = oecd))+
  geom_line( alpha = .2)+
  scale_color_manual(values = c("TRUE" = "darkorchid3", "FALSE" = "grey50"))+
  geom_point(data = d2 %>% filter(days_since_1case == 1), size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
  geom_point(data = d2 %>% filter(days_since_1death == 1), size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
  scale_x_continuous(breaks = seq(0,100, 25))+
  labs(
    title = "Timeline of COVID-19 deaths per 1 million"
    ,y = "Total Deaths per 1 million", x = "Days since first case outside of China (Jan 13, 2020)"
  )+
  geom_vline(xintercept = 58, linetype = "dotted")+
  geom_vline(xintercept = 75, linetype = "dashed")+
  geom_vline(xintercept = 100, linetype = "dashed", color = "red")
margings_for_plotly <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)
g2 <- plotly::ggplotly(g2)
g2 %>% plotly::layout(autosize = F, width = 900, height = 600, margin = margings_for_plotly)
# g2 %>% plotly::layout(autosize = T)

# ----- q3-toll-all-centered ----------------------
# 3. Repositioning to the first death:
d3 <- ds0 %>% filter(oecd)
g3 <- ds0 %>%
  # filter(country_code == "ITA") %>%
  ggplot(aes(x = days_since_exodus, y = n_deaths_cum_per_1m, group = country_label, color = oecd))+
  geom_line( alpha = .2)+
  scale_color_manual(values = c("TRUE" = "darkorchid3", "FALSE" = "grey50"))+
  geom_point(data = d3 %>% filter(days_since_1case == 1), size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
  geom_point(data = d3 %>% filter(days_since_1death == 1), size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
  scale_x_continuous(breaks = seq(-100,100, 25))+
  labs(
    title = "Timeline of COVID-19 deaths per 1 million (centered)"
    ,y = "Total Deaths (per 1 million)", x = "Days since first confirmed death in the country"
  )
margings_for_plotly <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)
g3 <- plotly::ggplotly(g3)
g3 %>% plotly::layout(autosize = F, width = 900, height = 600, margin = margings_for_plotly)
# g3 %>% plotly::layout(autosize = T)



# ----- q1-basic-timeline -------------
# ds0 %>% glimpse()
d1 <- ds0 %>% filter(oecd) %>%
  mutate(
    n_cases_cum = n_cases_cum / 1000
    ,n_cases_cum_per_1m = n_cases_cum_per_1m / 1000
  )
g1 <- d1 %>%
  ggplot(aes(
    x = days_since_exodus
    ,y = n_cases_cum
    # ,y =n_cases_cum_per_1m
    # ,y = n_deaths_cum
    # ,y = n_deaths_cum_per_1m
  ))+
  geom_line(size = .5)+
  # geom_line(aes(y=StringencyIndex), color = "red")+
  facet_wrap(~country_label, scale = "free", ncol = 6)+
  geom_point(
    data = d1 %>% filter(days_since_1case == 1)
    ,size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21
  )+
  geom_point(
    data = d1 %>% filter(days_since_1death == 1)
    ,size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21
  )+
  geom_vline(xintercept = 58, linetype = "dotted",)+
  geom_vline(xintercept = 75, linetype = "dashed", alpha = .5)+
  geom_vline(xintercept = 100, linetype = "dashed", color = "red", alpha = .5)+
  scale_x_continuous(breaks = seq(0,100, 50))+
  labs(
    title = "Timeline of COVID-19: Cumulative Cases"
    , y = "Cumulative Cases (in thousands)", x = "Days since first case outside of China (Jan 13, 2020)"
    , caption = "(first dot) = 1st confirmed case, (second dot) = 1st confirmed death,
    (dotted line) = pandemic announced by WHO, (dashed lines) = 75 and 100th day since Exodus"
  )
cat("\n## Cases\n")
g1
cat("\n## Cases per 1m\n")
g1 + aes(y = n_cases_cum_per_1m)+labs(y = "Cumulative Cases per 1 mil (in thousands)",
      title = "Timeline of COVID-19: Cumulative Cases per 1 million")
cat("\n## Deaths\n")
g1 + aes(y = n_deaths_cum)+labs(y = "Cumulative Deaths",
  title = "Timeline of COVID-19: Cumulative Deaths")
cat("\n## Deaths per 1m\n")
g1 + aes(y = n_deaths_cum_per_1m)+labs(y = "Cumulative Deaths per 1 mil",
   title = "Timeline of COVID-19: Cumulative Deaths per 1 million")

# ----- q1a -----------
# Q  How do key indices compare within each country?

focal_vars <- c( "n_cases_cum", "n_cases_cum_per_1m", "n_deaths_cum", "n_deaths_cum_per_1m",
                 "StringencyIndex")

ds1 <- ds0 %>%  filter(oecd) %>%
  # dplyr::filter(country_code %in% c("ITA","FRA")) %>%
  dplyr::select(country_code, country_label, days_since_exodus, days_since_1case,
                days_since_1death,
                n_cases_cum, n_cases_cum_per_1m, n_deaths_cum, n_deaths_cum_per_1m,
                StringencyIndex
  ) %>%
  tidyr::pivot_longer(cols = focal_vars, values_to = "value", names_to = "metric")

print_one_wrap <- function(d, country = "ITA"){
  # d <- ds1; country = "ITA"
  d1 <- d %>% filter(country_code == country) %>%
    dplyr::mutate(
      country_label = stringr::str_replace_all(country_label,"\\(the Republic of\\)","")
    )

  g1 <- d1 %>%
    ggplot(aes(x = days_since_exodus, y = value))+
    geom_line()+
    geom_point(
      data = d1 %>% filter(days_since_1case == 1),
      size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
    geom_point(
      data = d1 %>% filter(days_since_1death == 1),
      size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
    geom_vline(xintercept = 58, linetype = "dotted",)+
    geom_vline(xintercept = 75, linetype = "dashed", alpha = .5)+
    geom_vline(xintercept = 100, linetype = "dashed", color = "red", alpha = .5)+
    facet_wrap(country_label ~ metric, scale = "free_y",ncol = 5,
               labeller = labeller(metric = c(
                 "n_cases_cum" = " Cases",
                 "n_cases_cum_per_1m" = " Cases per million",
                 "n_deaths_cum" = "Deaths",
                 "n_deaths_cum_per_1m" = "Deaths per million",
                 "StringencyIndex" = "Stringency Index"
               )))+
    labs(y = NULL, x = NULL)
  g1
}

# ds1 %>% print_one_wrap(country = 'ITA')

countries <- unique(ds1$country_code)
for(country_i in countries){
  # cat("\n#### ", country_i,"\n")
  ds1 %>% print_one_wrap(country = country_i) %>% print()
  cat("\n")
}



# ----- q2-response-trend -----------------
# What the trend response to COVID-10 by each country?

d1 <- ds0 %>% filter(oecd)
g1 <- d1 %>%
  ggplot(aes(x = days_since_exodus, y = StringencyIndex))+
  geom_line()+
  geom_point(data = d1 %>% filter(days_since_1case == 1), size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
  geom_point(data = d1 %>% filter(days_since_1death == 1), size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
  facet_wrap(~country_label)+
  geom_vline(xintercept = 58, linetype = "dotted",)+
  geom_vline(xintercept = 75, linetype = "dashed", alpha = .5)+
  geom_vline(xintercept = 100, linetype = "dashed", color = "red", alpha = .5)+
  labs(
    title = "Timeline of OECD countries' respones to COVID-19 as measured by the Stringency Index"
    ,y = "Stringency Index", x = "Days since first case outside of China (Jan 13, 2020)"
    , caption = "(first dot) = 1st confirmed case, (second dot) = 1st confirmed death,
    (dotted line) = pandemic announced by WHO, (dashed lines) = 75 and 100th day since Exodus"
  )
cat("\n## Stringency\n")
g1
cat("\n")
cat("\n## Government Response\n")
g2 <- g1+aes(y = GovernmentResponseIndex)+labs(
  title = "Timeline of OECD countries' respones to COVID-19 as measured by the Government Response Index", y="Gov Response Index")
g2
cat("\n")
cat("\n## Containment\n")
g3 <- g1+aes(y = ContainmentHealthIndex)+labs(
  title = "Timeline of OECD countries' respones to COVID-19 as measured by the Containment  Index", y="Containment Index")
g3
cat("\n")
cat("\n## Economic Support\n")
g4 <- g1+aes(y = EconomicSupportIndex    )+labs(
  title = "Timeline of OECD countries' respones to COVID-19 as measured by the Economic Support  Index", y="Econ Support Index")
g4





# ---- publish ---------------------------------------
path_report <- "./analysis/eda-1/eda-1.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)


