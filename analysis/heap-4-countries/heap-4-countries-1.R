# This script reads two files: patient event table + location map.
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(forcats)
library(stringr)
library(lubridate)
library(knitr) # dynamic documents
library(rmarkdown) # dynamic
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
library(plotly)
# library(TabularManifest) # exploratory data analysis, see https://github.com/Melinae/TabularManifest
requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables

#---- load-sources ------------------------------------------------------------
config <- config::get()
source("./scripts/common-functions.R")        # reporting functions and quick views
# source("./scripts/graphing/graph-presets.R") # font and color conventions
# source("./scripts/graphing/graph-support.R") # font and color conventions

source("./analysis/us-response/cgrt-levels.R")
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
      ,n_deaths_cum_per_1m = n_deaths_cum/n_population*1000000
      ,n_cases_cum_per_1m  = n_cases_cum/ n_population*1000000

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


      ,days_since_10death   = cumsum(tidyr::replace_na(n_deaths_cum >= 10,0))
      ,days_since_100case    = cumsum(tidyr::replace_na(n_cases_cum >= 100,0))
      ,date_of_10death      = lubridate::as_date(ifelse(days_since_10death==1,date, NA))
      ,date_of_100case       = lubridate::as_date(ifelse(days_since_100case==1,date, NA))
      ,date_of_10death      = min(date_of_10death, na.rm =T)
      ,date_of_100case       = min(date_of_100case, na.rm =T)
      ,days_since_10death   = (date - date_of_10death) %>% as.integer()
      ,days_since_100case    = (date - date_of_100case) %>% as.integer()

      ,n_cases_roll_7 = zoo::rollapply(n_cases, 7, mean, align = 'right', fill = NA)
      ,n_deaths_roll_7 = zoo::rollapply(n_deaths, 7, mean, align = 'right', fill = NA)
      ,n_cases_roll_7_rate = n_cases_roll_7/n_population*1000000
      ,n_deaths_roll_7_rate = n_deaths_roll_7/n_population*1000000


    ) %>%
    dplyr::ungroup() %>%
    # dplyr::filter(epi_timeline > 0) %>%
    dplyr::mutate(
      days_since_exodus    = as.integer(date - lubridate::date("2020-01-13")) # first case outside of china
      ,days_since_pandemic = as.integer(date - lubridate::date("2020-03-11")) # WHO declares pandemic
    ) %>%
    select(-cutoff_death, - cutoff_case, -date_of_1death, -date_of_1case, -date_of_10death, -date_of_100case)
  return(d_out)
}

# for testing the function:
# d_out <- ds0 %>%  filter(country_code == "ITA") %>%
#     select(
#       country_code, date,n_cases, n_deaths, ConfirmedDeaths, ConfirmedCases
#     ) %>%
#   compute_epi_timeline()




# ds_daily %>% print_plotly_lines("confirmed", y  = "Confirmed Cases", title = "XXX")
# ds_daily %>% print_plotly_lines("active")
# ds_cgrt %>% print_plotly_lines("stringency_index",grouping = "region_code",  default_region = c("USA","GBR","IRL","CAN"))

# ---- load-data -------------------------------------------------------------
# reference table for geographic units
ds_geo <- readr::read_csv("./data-public/metadata/world-geography.csv")
ds_geo %>% glimpse()

ds_covid <- readr::read_csv(config$path_input_covid)
ds_covid %>% glimpse()

ds_who <- readr::read_csv(config$path_input_who)
ds_who %>% glimpse()

ds_lockdown <- readr::read_csv("./analysis/heap-4-countries/country-lockdown.csv")
ds_lockdown
# ---- tweak-data --------------------

focus_4_countries <- c("USA","GBR", "IRL","CAN")
# ds_covid %>% select(country_code) %>%
#   left_join(ds_geo %>% select(country_code, country_name) ) %>%
#   distinct() %>% arrange(country_name) %>% View()
ds_covid <- ds_covid %>%
  rename(n_population = n_population_2018) %>%
  filter(
    country_code %in% focus_4_countries
    # country_code %in% c("BEL", "CAN", "DEN", "DEU", "ESP", "FRA", "GBR", "IRL", "ITA", "NOR", "USA")
  )

ds_epi <- ds_covid %>%
  compute_epi_timeline() %>%
  select(-n_cases, -n_deaths)

ds_epi %>% glimpse()


ds_who <- ds_who %>%
  filter(
    country_code %in% focus_4_countries
  ) %>%
  select(date,country_code, country_label, n_cases, n_deaths, n_population = population)

ds_who %>% glimpse()

ds_epi_who <- ds_who %>%
  compute_epi_timeline() %>%
  select(-n_cases, -n_deaths)

ds_epi_who %>% glimpse()

ds_epi %>% distinct(country_code, country_label)


metric_order <- c(
  # "n_cases"           = "Cases (this day)"
  "n_cases_roll_7"   = "Cases (7-day average)"
  ,"n_cases_roll_7_rate" = "Cases (7DA/1M)"
  ,"n_cases_cum"      = "Cases (cumulative)"
  ,"n_cases_cum_per_1m"    = "Cases (cum/1M)"
  # ,"n_deaths"         = "Deaths (this day)"
  ,"n_deaths_roll_7"  = "Deaths (7-day average)"
  ,"n_deaths_roll_7_rate"  = "Deaths (7DA/1M)"
  ,"n_deaths_cum"     = "Deaths (cumulative)"
  ,"n_deaths_cum_per_1m"   = "Deaths (cum/1M)"
)

ds1 <- ds_epi %>%
  tidyr::pivot_longer(cols = names(metric_order), names_to = "metric", values_to = "value") %>%
  mutate(
    metric = factor(metric, levels = names(metric_order), labels = metric_order)
  ) %>%
  left_join(
    ds_geo %>% distinct(country_code, country_label)
  ) %>%
  left_join(
    ds_lockdown, by = c("date","country_code")
  ) %>%
  mutate_at(
    .vars = c("lockdown_type","lockdown_label")
    ,.funs = factor
  )
ds1 %>% glimpse()

ds1 %>% distinct(country_label)

# ds1a %>% distinct(country_code)
# ds1 <- ds1a %>%
#   filter(country_code %in% c("USA","GBR", "IRL","CAN"))
#

ds2 <- ds1 %>%
  filter(metric %in% c("Cases (cum/1M)", "Deaths (cum/1M)"))
ds2 %>% glimpse()


ds1_who <- ds_epi_who %>%
  tidyr::pivot_longer(cols = names(metric_order), names_to = "metric", values_to = "value") %>%
  mutate(
    metric = factor(metric, levels = names(metric_order), labels = metric_order)
  ) %>%
  left_join(
    ds_geo %>% distinct(country_code, country_label)
  ) %>%
  left_join(
    ds_lockdown, by = c("date","country_code")
  ) %>%
  mutate_at(
    .vars = c("lockdown_type","lockdown_label")
    ,.funs = factor
  )
ds1_who %>% glimpse()
ds1_who %>% distinct(country_label)


# ------ facet-graphs ----------------------

# d %>% glimpse()
print_epi <- function(d,xvar = "date"){
  # d <- ds1
  country_colors <- c("Ireland" = "#33a02c", "United Kingdom" = "#b2df8a", "United States" = "#a6cee3", "Canada" = "#1f78b4")
  country_linetype <- c("Ireland" = "dotted", "United Kingdom" = "dotdash", "United States" = "solid", "Canada" = "longdash")
  g <- d %>%
    ggplot(aes_string(x=xvar, y = "value", group = "country_label",color = "country_label",
                      linetype = "country_label"))+
    # geom_line()+
    # country_colors    geom_line(color = "black", size = .2, alpha = .3)+
    # geom_line(size = .3)+
    geom_line(size = .3)+
    geom_line(size = .3, color = "grey40")+
    geom_line(size = 1.5, alpha = .3, linetype = "solid")+
    scale_color_viridis_d(option = "plasma", begin = .2, end = .9, name = "Country")+
    # scale_color_viridis_d(option = "viridis", begin = .2, end = .9)+
    # scale_color_manual(values = country_colors, name = "Country")+
    scale_linetype_manual(values = country_linetype, name = "Country")+
    facet_wrap(~metric, scales = "free", ncol = 4)+
    scale_y_continuous(labels = scales::comma_format())+
    labs(y = NULL)

}
g1 <- ds1 %>% print_epi() + labs(x = "Date")
ggsave("./analysis/heap-4-countries/2020-11-11/date.jpg",g1,"jpg", width = 12, height = 6,dpi = "retina")


g1 <- ds1 %>% print_epi(xvar = "days_since_1case") + labs(x = "Days since 1st case")
ggsave("./analysis/heap-4-countries/2020-11-11/days_since_1case.jpg",g1,"jpg", width = 12, height = 6,dpi = "retina")

g1 <- ds1 %>% print_epi(xvar = "days_since_1death") + labs(x = "Days since 1st death")
ggsave("./analysis/heap-4-countries/2020-11-11/days_since_1death.jpg",g1,"jpg", width = 12, height = 6,dpi = "retina")

# ------ singular -----------------------

# d <- ds1
country_colors <- c("Ireland" = "#33a02c", "United Kingdom" = "#b2df8a", "United States" = "#a6cee3", "Canada" = "#1f78b4")
country_linetype <- c("Ireland" = "dotted", "United Kingdom" = "dotdash", "United States" = "solid", "Canada" = "longdash")


g2 <- ds1 %>%
  filter(metric %in% c("Cases (cum/1M)")) %>%
  ggplot(aes_string(x="date", y = "value", group = "country_label",color = "country_label",linetype = "country_label"))+
  geom_line(size = .5, color = "grey40")+
  geom_line(size = .5, alpha = .5)+
  geom_line(size = 3, alpha = .3, linetype = "solid")+
  scale_color_viridis_d(option = "plasma", begin = .2, end = .9, name = "Country")+
  scale_linetype_manual(values = country_linetype, name = "Country")+
  scale_y_continuous(labels = scales::comma_format(), breaks = seq(0, 32000, 2000), minor_breaks = seq(0,32000, 1000))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  labs(y = NULL)+
  labs(y = "Cases (cumulative, per 1 million)", x = "2020", title = "Confirmed cases of COVID-19", subtitle = "Cumulative count per 1 million of population", caption = "Source: European Centre for Disease Prevention and Control")
# g2
ggsave("./analysis/heap-4-countries/2020-11-11/cases_cum_per1m.jpg",g2,"jpg", width = 8, height = 6,dpi = "retina")



g3 <- ds1 %>%
  filter(metric %in% c("Deaths (cum/1M)")) %>%
  ggplot(aes_string(x="date", y = "value", group = "country_label",color = "country_label",linetype = "country_label"))+
  geom_line(size = .5, color = "grey40")+
  geom_line(size = .5, alpha = .5)+
  geom_line(size = 3, alpha = .3, linetype = "solid")+
  scale_color_viridis_d(option = "plasma", begin = .2, end = .9, name = "Country")+
  scale_linetype_manual(values = country_linetype, name = "Country")+
  scale_y_continuous(labels = scales::comma_format(),breaks = seq(0,800,100))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  labs(y = "Deaths (cumulative, per 1 million)", x = "2020", title = "Deaths from COVID-19", subtitle = "Cumulative count per 1 million of population", caption = "Source: European Centre for Disease Prevention and Control")
# g3
ggsave("./analysis/heap-4-countries/2020-11-11/deaths_cum_per1m.jpg",g3,"jpg", width = 8, height = 6,dpi = "retina")




g4 <- ds1 %>%
  filter(metric %in% c("Cases (7DA/1M)")) %>%
  ggplot(aes_string(x="date", y = "value", group = "country_label",color = "country_label",linetype = "country_label"))+
  geom_line(size = .5, color = "grey40")+
  geom_line(size = .5, alpha = .5)+
  geom_line(size = 3, alpha = .3, linetype = "solid")+
  scale_color_viridis_d(option = "plasma", begin = .2, end = .9, name = "Country")+
  scale_linetype_manual(values = country_linetype, name = "Country")+
  scale_y_continuous(labels = scales::comma_format(), breaks = seq(0,400, 20), minor_breaks = seq(0,400, 10))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  labs(y = NULL)+
  labs(y = "Cases (7-day average, per 1 million)", x = "2020", title = "Confirmed cases of COVID-19", subtitle = "7-day rolling average per 1 million of population", caption = "Source: European Centre for Disease Prevention and Control")
# g4
ggsave("./analysis/heap-4-countries/2020-11-11/cases_7da_per1m.jpg",g4,"jpg", width = 8, height = 6,dpi = "retina")


g5 <- ds1 %>%
  filter(metric %in% c("Deaths (7DA/1M)")) %>%
  ggplot(aes_string(x="date", y = "value", group = "country_label",color = "country_label",linetype = "country_label"))+
  geom_line(size = .5, color = "grey40")+
  geom_line(size = .5, alpha = .5)+
  geom_line(size = 3, alpha = .3, linetype = "solid")+
  scale_color_viridis_d(option = "plasma", begin = .2, end = .9, name = "Country")+
  scale_linetype_manual(values = country_linetype, name = "Country")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = seq(0,16,2), minor_breaks = seq(0,16, 1))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  labs(y = NULL)+
  labs(y = "Deaths (7-day average, per 1 million)", x = "2020", title = "Deaths from COVID-19", subtitle = "7-day rolling average per 1 million of population", caption = "Source: European Centre for Disease Prevention and Control")
# g5
ggsave("./analysis/heap-4-countries/2020-11-11/death_7da_per1m.jpg",g5,"jpg", width = 8, height = 6,dpi = "retina")


# ----- gemma-1 -----------------------
library(scales)
ds1 %>% glimpse()

country_visuals <- tibble::tribble(
  ~country_label,      ~country_color, ~country_linetype
  ,"Ireland"          ,"#1B9E77"   , "dotted"
  ,"United Kingdom"   ,"#D95F02"     , "dotdash"
  ,"United States"    ,"#7570B3"      , "solid"
  ,"Canada"           ,"#E7298A"      , "longdash"
  ,"Belgium"          ,"grey90"       , "solid"
  ,"Germany"          ,"grey90"       , "solid"
  ,"Spain"            ,"grey90"       , "solid"
  ,"France"           ,"grey90"       , "solid"
  ,"Italy"            ,"grey90"       , "solid"
  ,"Norway"           ,"grey90"       , "solid"
)
country_colors <- country_visuals %>% pull(country_color)
names(country_colors) <- country_visuals %>%  pull(country_label)
country_linetype <- country_visuals %>% pull(country_linetype)
names(country_linetype) <- country_visuals %>%  pull(country_label)

ds_lockdown
d6 <-
  ds1 %>% # ECCD
  # ds1_who %>%
  # filter(days_since_100case <= 50) %>%
  filter(metric %in% c("Cases (cumulative)"))

g6 <- d6 %>%
  ggplot(aes_string(x="days_since_100case", y = "value", group = "country_label",color = "country_label",linetype = "country_label"))+
  geom_line(size = .5, color = "grey40")+
  geom_line(size = .5, alpha = .5)+
  geom_line(size = 3, alpha = .3, linetype = "solid")+
  geom_point(shape = 21,size =3, data = d6 %>% filter(!is.na(lockdown_type)) )+
  # geom_point(aes(fill = country_label),shape = 21,size =2.5, alpha =.4, color = "black",data = d6 %>% filter(!is.na(lockdown_type)) )+
  # geom_point(aes(shape = lockdown_type))+
  # scale_color_viridis_d(option = "plasma", begin = .2, end = .9, name = "Country")+
  scale_linetype_manual(values = country_linetype, name = "Country")+
  scale_color_manual(values = country_colors, name = "Country")+
  scale_fill_manual(values = country_colors, name = "Country")+
  scale_y_log10(
    breaks = c(100,1000, 10000, 100000, 1000000)
    ,labels = comma
    ,limits = c(100, 10000000)
  ) +
  scale_x_continuous(limits = c(0,250))+
# g6
  labs(y = "Cases (cumulative)", x = "Days since 100th case", title = "Confirmed cases of COVID-19", subtitle = "Cumulative count "
       # , caption = "Source: World Health Organization"
       , caption = "Source: European Centre for Disease Control"
       )
# g2
ggsave(
  # "./analysis/heap-4-countries/2020-11-12/cases_cum_log-WHO.jpg"
  "./analysis/heap-4-countries/2020-11-12/cases_cum_log-ECDC.jpg"
  ,g6,"jpg", width = 9, height = 6,dpi = "retina")


# ---- publish ---------------------------------------
path_report <- "./analysis/us-response/us-response-2.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)


path_report <- "./analysis/us-response/us-response-2.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)




