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
# list of focal countries in OECD database
ds_country <-
  readr::read_csv(
    config$path_country
  ) %>%
  dplyr::filter(desired)
#
# # ECDC
# # path_save <- paste0("./data-unshared/derived/ocdc-",Sys.Date(),".csv")
ds_covid <- readr::read_csv(config$path_input_covid,)
# ds_covid %>% glimpse()

ds_country_codes <- readr::read_csv(config$path_country_codes)

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
# to keep it manageble
ds_cgrt <- ds_cgrt %>%
  select(country_code, date, StringencyIndex )
# ds_cgrt %>% glimpse()
# n_distinct(ds_cgrt$country_code)
# ds_covid$country_code %>% unique() %>% length()
# ds_cgrt$country_code %>% unique() %>% length()

# ---- inspect-data ----------------------

# ---- tweak-data-1 ---------------
ds0 <- ds_covid %>%
  compute_epi_timeline() %>%
  dplyr::left_join(
    ds_cgrt
    ,by = c("date", "country_code")
  ) %>%
  dplyr::left_join(
    ds_country_codes,
    by = c("country_code" = "country_code3")
  )

# ds0 %>% glimpse()
# ds0$country_code %>% unique() %>% length()
# d_out <- ds0 %>% filter(country_code == "ITA")
# d_out <- ds0 %>% filter(country_code == "LVA")

# ----current-toll ----------------

d1 <- ds0 %>%
  filter(country_code %in% ds_country$id) %>%
  filter(date == lubridate::as_date("2020-05-17"))
# d1 %>% glimpse()

g1 <- d1 %>%
  mutate(country_label = forcats::fct_reorder(country_label, n_deaths_cum_per_1m)) %>%
  ggplot(aes(x = n_deaths_cum_per_1m, y = country_label))+
  geom_segment(aes(yend = country_label, xend = min(n_deaths_cum_per_1m)))+
  geom_point(aes(size = n_cases_cum_per_1m), shape = 21, alpha = .2 )
g1


d1 <- ds0 %>%
  filter(country_code %in% ds_country$id) %>%
  filter(days_since_1death == 60) %>%
  mutate(country_label = forcats::fct_reorder(country_label, n_deaths_cum_per_1m)) %>%
  ggplot(aes(x = n_deaths_cum_per_1m, y = country_label))+
  geom_segment(aes(yend = country_label, xend = min(n_deaths_cum_per_1m)))+
  geom_point(aes(size = n_cases_cum_per_1m), shape = 21, alpha = .2 )
g1


d1 <- ds0 %>%
  filter(country_code %in% ds_country$id) %>%
  filter(date == lubridate::as_date("2020-05-17")) %>%
  mutate(since_exodus_to_first_death = days_since_exodus - days_since_1death) %>%
  mutate(since_exodus_to_first_case = days_since_exodus - days_since_1case)
d1 %>% glimpse()
d1 %>%
  ggplot(aes( x = since_exodus_to_first_case, y = n_deaths_cum_per_1m)) +
  # ggplot(aes( x = since_exodus_to_first_death, y = n_deaths_cum_per_1m)) +
  geom_text(aes(label = country_code))


# ---- covid-metric-1 -----------------------
# d1 <- ds0 %>%
d_out <- ds0 %>%
  filter(country_code == "ITA") %>%
  select(country_code, date,n_cases_cum, n_deaths_cum, days_since_1case, days_since_1death)

# Deaths 30 days after 1st death
d1 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1death == 30) %>%
  dplyr::select(country_code, n_deaths_cum, n_deaths_cum_per_1m) %>%
  dplyr::rename(n_deaths_30days_since_1death       = n_deaths_cum) %>%
  dplyr::rename(n_deaths_30days_since_1death_per1m = n_deaths_cum_per_1m) %>%
  ungroup()
d1 %>% glimpse()
# Deaths 60 days after 1st death
d2 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1death == 60) %>%
  dplyr::select(country_code, n_deaths_cum,n_deaths_cum_per_1m) %>%
  dplyr::rename(n_deaths_60days_since_1death       = n_deaths_cum) %>%
  dplyr::rename(n_deaths_60days_since_1death_per1m = n_deaths_cum_per_1m)%>%
  ungroup()

# Cases 30 days after 1st case
d3 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1case == 30) %>%
  dplyr::select(country_code, n_cases_cum, n_cases_cum_per_1m) %>%
  dplyr::rename(n_cases_30days_since_1case       = n_cases_cum) %>%
  dplyr::rename(n_cases_30days_since_1case_per1m = n_cases_cum_per_1m)%>%
  ungroup()

# Cases 60 days after 1st case
d4 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1case == 60) %>%
  dplyr::select(country_code, n_cases_cum, n_cases_cum_per_1m) %>%
  dplyr::rename(n_cases_60days_since_1case       = n_cases_cum) %>%
  dplyr::rename(n_cases_60days_since_1case_per1m = n_cases_cum_per_1m) %>%
  ungroup()


# Deaths 30 days after 1st case
d5 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1case == 30) %>%
  dplyr::select(country_code, n_deaths_cum, n_deaths_cum_per_1m) %>%
  dplyr::rename(n_deaths_30days_since_1case       = n_deaths_cum) %>%
  dplyr::rename(n_deaths_30days_since_1case_per1m = n_deaths_cum_per_1m) %>%
  ungroup()
d5 %>% glimpse()
# Deaths 60 days after 1st case
d6 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1case == 60) %>%
  dplyr::select(country_code, n_deaths_cum, n_deaths_cum_per_1m) %>%
  dplyr::rename(n_deaths_60days_since_1case       = n_deaths_cum) %>%
  dplyr::rename(n_deaths_60days_since_1case_per1m = n_deaths_cum_per_1m)%>%
  ungroup()

# Cases 30 days after 1st death
d7 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1death == 30) %>%
  dplyr::select(country_code, n_cases_cum, n_cases_cum_per_1m) %>%
  dplyr::rename(n_cases_30days_since_1death = n_cases_cum) %>%
  dplyr::rename(n_cases_30days_since_1death_per1m = n_cases_cum_per_1m)%>%
  ungroup()

# Cases 60 days after 1st case
d8 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1death == 60) %>%
  dplyr::select(country_code, n_cases_cum, n_cases_cum_per_1m) %>%
  dplyr::rename(n_cases_60days_since_1death = n_cases_cum) %>%
  dplyr::rename(n_cases_60days_since_1death_per1m = n_cases_cum_per_1m) %>%
  ungroup()


# Deaths 100 days since exodus
d9 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_exodus == 100) %>%
  dplyr::select(country_code, n_deaths_cum, n_deaths_cum_per_1m) %>%
  dplyr::rename(n_deaths_100days_since_exodus       = n_deaths_cum) %>%
  dplyr::rename(n_deaths_100days_since_exodus_per_1m = n_deaths_cum_per_1m)%>%
  ungroup()


# Cases 100 days since exodus
d10 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_exodus == 100) %>%
  dplyr::select(country_code, n_cases_cum, n_cases_cum_per_1m) %>%
  dplyr::rename(n_cases_100days_since_exodus = n_cases_cum) %>%
  dplyr::rename(n_cases_100days_since_exodus_per_1m = n_cases_cum_per_1m) %>%
  ungroup()


# ds0 %>% filter(country_ == "LVA")
ds_scince_metric <- list(d1,d2,d3,d4,d5,d6,d7,d8,d9, d10) %>% Reduce(function(a,b) dplyr::full_join(a,b), .)
ds_scince_metric %>% glimpse()
longer_names <- setdiff(names(ds_scince_metric),"country_code")
ds_scince_metric_long <- ds_scince_metric %>%
  tidyr::pivot_longer(cols = longer_names, names_to = "metric", values_to = "value")
# ds_scince_metric_long %>% glimpse()

ds_scince_metric <- ds_scince_metric %>%
  dplyr::left_join(
    ds_country_codes, by = c("country_code" = "country_code3")
  ) %>%
  dplyr::filter(!is.na(country_code))

ds_scince_metric_long <- ds_scince_metric_long %>%
  dplyr::left_join(
    ds_country_codes, by = c("country_code" = "country_code3")
  ) %>%
  dplyr::filter(!is.na(country_code))

# ds_scince_metric_long %>% glimpse()
# ds_scince_metric %>% glimpse()

# ds_scince_metric %>% neat_DT()
ls_scince_metric <- list(
  "wide" = ds_scince_metric, "long" = ds_scince_metric_long
)
ls_scince_metric %>%  readr::write_rds("./analysis/shiny-since-metric/data.rds")


# ----- why_75-1 ----------------------

# Why 75 days after exodus should be the starting point?
# 1. Most countries have peaked in their response
d1 <- ds0 %>%
  filter(country_code %in% ds_country$id)
g1 <- d1 %>%
  # filter(country_code %in% ds_country$id) %>%
  # filter(country_code == "ITA") %>%
  ggplot(aes(x = days_since_exodus, y = StringencyIndex, group = country_label))+
  geom_line( alpha = .2)+
  geom_point(data = d1 %>% filter(days_since_1case == 1), size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
  geom_point(data = d1 %>% filter(days_since_1death == 1), size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
  scale_x_continuous(breaks = seq(0,100, 25))+
  labs(
    title = "Timeline of OECD countries' respones to COVID-19 as measured by the Stringency Index"
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

# ----- why_75-2 ----------------------
# 2. This is when the mortality curves starts going up
d2 <- ds0 %>%
  filter(country_code %in% ds_country$id)
g2 <- d2 %>%
  # filter(country_code %in% ds_country$id) %>%
  # filter(country_code == "ITA") %>%
  ggplot(aes(x = days_since_exodus, y = n_deaths_cum_per_1m, group = country_label))+
  geom_line( alpha = .2)+
  geom_point(data = d2 %>% filter(days_since_1case == 1), size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
  geom_point(data = d2 %>% filter(days_since_1death == 1), size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
  scale_x_continuous(breaks = seq(0,100, 25))+
  labs(
    title = "Timeline of COVID-19 among OECD countries"
    ,y = "Total Deaths per 1 million", x = "Days since first case outside of China (Jan 13, 2020)"
  )+
  geom_vline(xintercept = 58, linetype = "dotted")+
  geom_vline(xintercept = 75, linetype = "dashed")+
  geom_vline(xintercept = 100, linetype = "dashed", color = "red")
g2 <- plotly::ggplotly(g2)
g2 %>% plotly::layout(autosize = F, width = 900, height = 600, margin = margings_for_plotly)


# ----- why_75-3 ----------------------
# 3. Repositioning to the first death:
d3 <- ds0 %>%
  filter(country_code %in% ds_country$id)
g3 <- d3 %>%
  # filter(country_code %in% ds_country$id) %>%
  # filter(country_code == "ITA") %>%
  ggplot(aes(x = days_since_1death, y = n_deaths_cum_per_1m, group = country_label))+
  geom_line( alpha = .2)+
  geom_point(data = d3 %>% filter(days_since_1case == 1), size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
  geom_point(data = d3 %>% filter(days_since_1death == 1), size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
  scale_x_continuous(breaks = seq(-100,100, 25))+
  labs(
    title = "Timeline of COVID-19 among OECD countries"
    ,y = "Total Deaths (per 1 million)", x = "Days since first confirmed death in the country"
  )
g3 <- plotly::ggplotly(g3)
g3 %>% plotly::layout(autosize = F, width = 900, height = 600, margin = margings_for_plotly)


# ----- why_75-4 ----------------------

# 4. Regradless of scale, we see that Day 75 (2020-03-28) is approximately the scree point in the mortality trajectory
# ds0 %>% glimpse()
d4 <- ds0 %>%
  filter(country_code %in% ds_country$id)
g4 <- d4 %>%
  ggplot(aes(
    x = days_since_exodus
    ,y = n_deaths_cum_per_1m
  ))+
  geom_line()+
  # geom_line(aes(y=StringencyIndex), color = "red")+
  facet_wrap(~country_label, scale = "free")+
  geom_point(data = d4 %>% filter(days_since_1case == 1), size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
  geom_point(data = d4 %>% filter(days_since_1death == 1), size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
  scale_x_continuous(breaks = seq(0,100, 25))+
  labs(
    title = "Timeline of COVID-19 among OECD countries"
    , y = "Total Deaths (per 1 million)", x = "Days since first case outside of China (Jan 13, 2020)"
    , caption = "(first dot) = 1st confirmed case, (second dot) = 1st confirmed death, \n(dotted line) = pandemic announced by WHO, (dashed line) = March 28 (75 days since exodus)"
  )+
  geom_vline(xintercept = 58, linetype = "dotted")+
  geom_vline(xintercept = 75, linetype = "dashed")+
  geom_vline(xintercept = 100, linetype = "dashed", color = "red")
g4
# ----- why_75-5 ----------------------
g5 <- g4 +aes(y = n_deaths_cum)+labs(y = "Total Deaths")
g5

# ----- ----------------------
featured_metrics <- c(
  "n_deaths_30days_since_1death_per1m" = "Deaths 30 days since 1st death (per 1m)"
  ,"n_deaths_30days_since_1case_per1m" = "Deaths 30 days since 1st case (per 1m)"
  ,"n_cases_30days_since_1death_per1m" = "Cases 30 days since 1st death (per 1m"
  ,"n_cases_30days_since_1case_per1m"  = "Cases 30 days since 1st case (per 1m)"
)
ds_scince_metric_long %>% glimpse()
ds_scince_metric %>% glimpse()


ds_scince_metric %>%
  filter(country_code %in% ds_country$id) %>%
  TabularManifest::histogram_continuous("n_deaths_30days_since_1death_per1m")

g1 <- ds_scince_metric %>%
  filter(country_code %in% ds_country$id) %>%
  ggplot(aes_string(
    x = "n_cases_30days_since_1case"
    ,y = "n_deaths_30days_since_1case"
    , label = "country_code"
    , group = "country_label"
    )) +
  geom_text()
g2 <- g1 + aes_string(
  x  = "n_cases_30days_since_1death"
  ,y = "n_deaths_30days_since_1death"
)
g1 <-   plotly::ggplotly(g1)
g2 <-   plotly::ggplotly(g2)

plotly::subplot(g1, g2, nrows =1)
library(cowplot)
cowplot::plot_grid( as_grob(g1), as_grob(g2))

g <- ds_wide %>%
  ggplot(aes_string(
    x = var_name_x
    , y = var_name_y
    , label = "contry_code2"
    , group = "country_label"))+
  geom_text()+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    , vjust = 3
  )
g <- plotly::ggplotly(g)
g %>% print()




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
  )

ds_citserv %>% glimpse()

ds1_long <- ds_scince_metric_long %>%
  select(country_code, metric, value) %>%
  # filter(country_code == "ITA") %>%
  bind_rows(ds_citserv ) %>%
  dplyr::left_join(
    ds_country_codes, by = c("country_code" = "country_code3")
  ) %>%
  mutate(
    metric = stringr::str_replace_all(metric," ","_")
    ,metric = stringr::str_replace_all(metric,"\\(","_")
    ,metric = stringr::str_replace_all(metric,"\\)","_")
    ,metric = stringr::str_replace_all(metric,"\\%","_")
    ,metric = stringr::str_replace_all(metric,",","_")
    ,metric = stringr::str_replace_all(metric,"-","_")

  )
ds1_long$metric %>% unique()

ds1_wide <- ds1_long %>%
  tidyr::pivot_wider(names_from = "metric", values_from = "value" )

list(
  "wide" = ds1_wide, "long" = ds1_long
) %>%
  readr::write_rds("./analysis/shiny-since-metric/data.rds")


# ---- publish ---------------------------------------
path_report <- "./analysis/response-stringency-1/response-stringency-1.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)


