# This script reads two files: patient event table + location map.
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(forcats)
library(stringr)
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

# margings_for_plotly <- list(
#   l = 50,
#   r = 50,
#   b = 100,
#   t = 100,
#   pad = 4
# )\


print_plotly_lines <- function(d, measure = "confirmed", grouping = "province_state", default_region = "Florida", ...){
  # d <- ds_daily
  # grouping_enq <-grouping_name rlang::sym(paste0("~ ",grouping))
  grouping_name <- parse(text = grouping)
  grouping_enq  <- rlang::sym(grouping)

  g1 <- d %>%
    # plotly::highlight_key(~ province_state) %>%
    # plotly::highlight_key(grouping_enq ) %>%
    plotly::highlight_key(~eval(grouping_name) ) %>%
    ggplot(aes_string(x = "date", y = measure, group = grouping))+
    # ggplot(aes(x = date, y = measure, group = !!grouping_enq))+
    geom_line( alpha = .3) +
    scale_y_continuous(labels = scales::comma)+
    labs(
      x = "Date", ...

    )
  # g1
  g1p <-
    plotly::ggplotly(g1) %>%                    # make into a plotly object
    plotly::highlight(                         # add highlight functionality
      on             = "plotly_click"          # or "plotly_hover"
      ,dynamic       = TRUE                    # adds color option
      ,selectize     = TRUE                    # select what to highlight
      ,persistent = TRUE
      ,defaultValues = default_region          # highlights in the beginning
    ) %>%
    plotly::layout(margin = list(l = 0, r = 0, b = 80, t = 30, pad = 0))
  # plotly::layout(margin = margings_for_plotly)
  g1p

}



# ds_cgrt %>% glimpse()
print_tile <- function(d, region, measure, relative_h = c(2,1)){
  # d <-  ds_cgrt
  # region = "United States"
  # measure = "c2"
  # relative_h = c(2,1)
  measure_str <-  meta_cgrt(measure,"name")
  measure_label <-  meta_cgrt(measure,"label")
  measure_enq <- rlang::sym(measure_str)


  main_title = paste0("(",toupper(region),") - ", measure_label)
  items_with_flag <- cgrt_key %>% filter(flag != "NULL") %>% pull(id)
  item_flag_name <- cgrt_key %>% filter(id == measure) %>% pull(flag) %>% tolower()
  item_flag_name_eqn <- rlang::sym(item_flag_name)
  d1 <- d %>%
    filter(region_name == region) #%>%
    # dplyr::rename(flag = c1_flag)
  # select(date,region_name, !!measure_enq)
  # d1
  g <- d1 %>%
    ggplot(aes(x=day, y = month, fill = !!measure_enq))+
    geom_tile(color = "white")+
    scale_fill_viridis_d(option = "magma", begin = .0, end = .9,  direction = -1,drop = FALSE,
                         na.translate = F)+
    # geom_point(aes(shape = factor(c2_flag)))+
    # scale_shape_manual(values = c("0" = "32", "1" = "8"))+
    theme(
      panel.grid = element_blank()
      # ,legend.position = "right"
    )+

    labs(y = NULL, x = "Day of the month",
         title = main_title, fill = meta_cgrt(measure, "label"))

  # g


  # if(measure %in% items_with_flag){
  #
  #   g <- g+geom_point(aes())
  # }

  g_legend <- ggpubr::get_legend(g) %>% ggpubr::as_ggplot()
  g <- cowplot::plot_grid(
    g +theme(legend.position = "none")
    , g_legend,ncol=1, rel_heights = relative_h
  )
  g
}
# ds_cgrt%>% print_tile("Ireland","c2")
# ds_cgrt%>% print_tile("United States","c2")
# ds_cgrt%>% print_tile("Canada","c2")
# ds_cgrt%>% print_tile("United Kingdom","c2")



# ds_daily %>% print_plotly_lines("confirmed", y  = "Confirmed Cases", title = "XXX")
# ds_daily %>% print_plotly_lines("active")
# ds_cgrt %>% print_plotly_lines("stringency_index",grouping = "region_code",  default_region = c("USA","GBR","IRL","CAN"))

# ---- load-data -------------------------------------------------------------
ds_daily <- readr::read_csv(config$path_input_jh_daily)
# ds_daily %>% glimpse()
ds_usts <- readr::read_csv(config$path_input_jh_usts)

# Total US population estimate
ds_us_pop <- readr::read_rds("./data-public/derived/us-pop-estimate.rds")

# ---- tweak-data --------------------
ds_us_pop <- ds_us_pop %>%
  select(sumlev, region, division, state, county,fips, state_name = stname, county_name = ctyname, popestimate2016, popestimate2019)

ds_us_pop_state <- ds_us_pop %>%
  filter(sumlev == "040") %>%
  select(-sumlev) %>%
  left_join(tibble::tibble(state_name = state.name, state_abb = state.abb)) %>%
  mutate(stabb = ifelse(state_name == "District of Columbia","DC",state_abb))


varnames_crouise <- c("Diamond Princess","Grand Princess" )
varnames_territories <- c(
  "American Samoa"
  ,"Guam"
  ,"Northern Mariana Islands"
  ,"Puerto Rico"
  ,"Virgin Islands"
)

ds_usts2 <- ds_usts %>%
  left_join(
    ds_us_pop %>% distinct(region, division,state_name)
    , by  = c("province_state" = "state_name")
  ) %>%
  dplyr::mutate(
    region2 = dplyr::case_when(
      (province_state %in% varnames_crouise) ~ "Cruiseship",
      (province_state %in% varnames_territories) ~ "Territories",
      TRUE ~ region
      ) %>% as_factor(),
    division2 = dplyr::case_when(
      province_state %in% varnames_crouise ~ "Cruiseship",
      province_state %in% varnames_territories ~ "Territories",
      TRUE ~ division
    ) %>% as_factor()
  )
ds_usts2 %>% glimpse()

ds_usts2 %>% distinct(region, region2)

#
ds_usts %>% filter(is.na(region)) %>% distinct(province_state)
# -----inspect ---------------

ds_daily %>% glimpse()
ds_usts %>% glimpse()
ds_us_pop %>% glimpse()
ds_us_pop_state %>% glimpse()
# ------ ----------------------



compute_epi <- function(d, grouping_vars){
  # d <- ds_daily
  # grouping_vars <- c("fips","date")
  grouping_vars_enquo <- rlang::syms(grouping_vars)

  d1 <- d %>%
    dplyr::arrange(!!!grouping_vars_enquo) %>%
    dplyr::group_by(!!!grouping_vars_enquo) %>%
    dplyr::summarize(
      confirmed_cum = sum(confirmed_cum, na.rm = T)
      ,deaths_cum = sum(deaths_cum, na.rm = T)
      ,tested_cum = sum(tested_cum, na.rm = T)
      ,hospitalized_cum = sum(hospitalized_cum, na.rm = T)
      ,population = sum(population, na.rm = T)
      ,incident_rate = confirmed_cum/population*100000
      ,mortality_rate = deaths_cum*100/population
      ,testing_rate = tested_cum/population*100000
      ,hospitalization_rate = hospitalized_cum/confirmed_cum
    ) %>%
    dplyr::mutate(
      confirmed       = confirmed_cum - lag(confirmed_cum,1)
      ,deaths         = deaths_cum - lag(deaths_cum,1)
      ,tested         = tested_cum - lag(tested_cum,1)
      ,hospitalized   = hospitalized_cum - lag(hospitalized_cum,1)
    ) %>%
    dplyr::mutate(
      confirmed_roll_7       = zoo::rollapply(confirmed, 7, mean, align = 'right', fill = NA)
      ,deaths_roll_7         = zoo::rollapply(deaths, 7, mean, align = 'right', fill = NA)
      ,tested_roll_7         = zoo::rollapply(tested, 7, mean, align = 'right', fill = NA)
      ,hospitalized_roll_7   = zoo::rollapply(hospitalized, 7, mean, align = 'right', fill = NA)
    ) %>%
    ungroup()
  d1
}

ds_daily %>% compute_epi(c("fips","date"))
ds_daily %>% compute_epi(c("fips"))

# ----- by-state ------------------
# ds_daily %>% glimpse()

ds_cov_state <- ds_daily %>%
  select(date,fips, confirmed_cum = confirmed, deaths_cum = deaths, tested_cum = people_tested, hospitalized_cum = people_hospitalized, population) %>%
  arrange(fips, date) %>%
  group_by(fips,date) %>%
  summarize(
    confirmed_cum = sum(confirmed_cum, na.rm = T)
    ,deaths_cum = sum(deaths_cum, na.rm = T)
    ,tested_cum = sum(tested_cum, na.rm = T)
    ,hospitalized_cum = sum(hospitalized_cum, na.rm = T)
    ,population = sum(population, na.rm = T)
    ,incident_rate = confirmed_cum/population*100000
    ,mortality_rate = deaths_cum*100/population
    ,testing_rate = tested_cum/population*100000
    ,hospitalization_rate = hospitalized_cum/confirmed_cum
  ) %>%
  mutate(
    confirmed       = confirmed_cum - lag(confirmed_cum,1)
    ,deaths         = deaths_cum - lag(deaths_cum,1)
    ,tested         = tested_cum - lag(tested_cum,1)
    ,hospitalized   = hospitalized_cum - lag(hospitalized_cum,1)
  ) %>%
  mutate(
    confirmed_roll_7       = zoo::rollapply(confirmed, 7, mean, align = 'right', fill = NA)
    ,deaths_roll_7         = zoo::rollapply(deaths, 7, mean, align = 'right', fill = NA)
    ,tested_roll_7         = zoo::rollapply(tested, 7, mean, align = 'right', fill = NA)
    ,hospitalized_roll_7   = zoo::rollapply(hospitalized, 7, mean, align = 'right', fill = NA)
  ) %>%
  ungroup() %>%
  left_join(
    ds_county_fips %>% distinct(fips = as.integer(fips), state_name, state_abb, region)
  ) %>%
  select(date, fips, state_name, state_abb, region, everything())

# ds_cov_state %>% glimpse()

# ----- by-state-prints ------------------
cat("\n## Cases\n")
g1 <- ds_cov_state %>%
  filter(!is.na(region)) %>%
  ggplot(aes(x = date, y = confirmed_roll_7, group = fips, color = region)) +
  geom_line()+
  # facet_wrap(~region, scale = "free")+
  facet_wrap(~region)+
  scale_color_viridis_d(option = "inferno", begin = .0, end = .8)+
  scale_y_continuous(label = scales::comma)+
  geom_text(aes(label = state_abb), data = ds_cov_state %>% filter(date == max(date), !is.na(region)) )+
  labs(title = "Running 7-day average of confirmed cases by state")
       # , y = "Confirmed cases (7-day average)")
g1
g1 + facet_wrap(~region, scale = "free")

cat("\n## Deaths\n")
g2 <- g1 + aes(y = deaths_roll_7)+labs(title = "Running 7-day average of deaths by state")
g2
g2 + facet_wrap(~region, scale = "free")

cat("\n## Tests\n")
g3 <- g1 + aes(y = tested_roll_7)+labs(title = "Running 7-day average of tests by state")
g3
g3 + facet_wrap(~region, scale = "free")

# ----- by-region --------------

ds_cov_region <- ds_daily %>%
  select(date,fips, confirmed_cum = confirmed, deaths_cum = deaths, tested_cum = people_tested, hospitalized_cum = people_hospitalized, population) %>%
  left_join(
    ds_county_fips %>% distinct(fips = as.integer(fips), state_name, state_abb, region)
  ) %>%
  arrange(fips, date) %>%
  group_by(region, date) %>%
  summarize(
    confirmed_cum = sum(confirmed_cum, na.rm = T)
    ,deaths_cum = sum(deaths_cum, na.rm = T)
    ,tested_cum = sum(tested_cum, na.rm = T)
    ,hospitalized_cum = sum(hospitalized_cum, na.rm = T)
    ,population = sum(population, na.rm = T)
    ,incident_rate = confirmed_cum/population*100000
    ,mortality_rate = deaths_cum*100/population
    ,testing_rate = tested_cum/population*100000
    ,hospitalization_rate = hospitalized_cum/confirmed_cum
  ) %>%
  mutate(
    confirmed       = confirmed_cum - lag(confirmed_cum,1)
    ,deaths         = deaths_cum - lag(deaths_cum,1)
    ,tested         = tested_cum - lag(tested_cum,1)
    ,hospitalized   = hospitalized_cum - lag(hospitalized_cum,1)
  ) %>%
  mutate(
    confirmed_roll_7       = zoo::rollapply(confirmed, 7, mean, align = 'right', fill = NA)
    ,deaths_roll_7         = zoo::rollapply(deaths, 7, mean, align = 'right', fill = NA)
    ,tested_roll_7         = zoo::rollapply(tested, 7, mean, align = 'right', fill = NA)
    ,hospitalized_roll_7   = zoo::rollapply(hospitalized, 7, mean, align = 'right', fill = NA)
  ) %>%
  ungroup() %>%
  select(date, region, everything())

# ds_cov_region %>% glimpse()

ds_cov_us <- ds_daily %>%
  select(date,confirmed_cum = confirmed, deaths_cum = deaths, tested_cum = people_tested, hospitalized_cum = people_hospitalized, population) %>%
  arrange(date) %>%
  group_by(date) %>%
  summarize(
    confirmed_cum = sum(confirmed_cum, na.rm = T)
    ,deaths_cum = sum(deaths_cum, na.rm = T)
    ,tested_cum = sum(tested_cum, na.rm = T)
    ,hospitalized_cum = sum(hospitalized_cum, na.rm = T)
    ,population = sum(population, na.rm = T)
    ,incident_rate = confirmed_cum/population*100000
    ,mortality_rate = deaths_cum*100/population
    ,testing_rate = tested_cum/population*100000
    ,hospitalization_rate = hospitalized_cum/confirmed_cum
  ) %>%
  mutate(
    confirmed       = confirmed_cum - lag(confirmed_cum,1)
    ,deaths         = deaths_cum - lag(deaths_cum,1)
    ,tested         = tested_cum - lag(tested_cum,1)
    ,hospitalized   = hospitalized_cum - lag(hospitalized_cum,1)
  ) %>%
  mutate(
    confirmed_roll_7       = zoo::rollapply(confirmed, 7, mean, align = 'right', fill = NA)
    ,deaths_roll_7         = zoo::rollapply(deaths, 7, mean, align = 'right', fill = NA)
    ,tested_roll_7         = zoo::rollapply(tested, 7, mean, align = 'right', fill = NA)
    ,hospitalized_roll_7   = zoo::rollapply(hospitalized, 7, mean, align = 'right', fill = NA)
  ) %>%
  ungroup() %>%
  select(date, everything())

# ds_cov_us %>% glimpse()

ds_cov_combined <-
  ds_cov_us %>% mutate(region = "USA") %>%
  bind_rows(ds_cov_region)
# ---- by-region-prints --------------
# Cases
cat("\n## Cases\n")
g1 <- ds_cov_combined %>%
  filter(!is.na(region)) %>%
  ggplot(aes(x = date, y = confirmed_roll_7, group = region, color = region)) +
  geom_line(size = 1)+
  scale_color_viridis_d(option = "inferno", begin = .0, end = .9)+
  scale_y_continuous(label = scales::comma)+
  labs(color = "Region")
g1 + labs(title = "Rolling average (7-day) of confirmed cased by region")
g1 + facet_wrap(~region, scale = "free")+ labs(title = "Rolling average (7-day) of confirmed cased by region")
g1 + aes(y = incident_rate)+ labs(title = "Cases per 100,000 persons by region")

cat("\n## Deaths\n")

g1 +aes(y = deaths_roll_7)+ labs(title = "Rolling average (7-day) of deaths by region")
g1 +aes(y = deaths_roll_7)+ facet_wrap(~region, scale = "free")+ labs(title = "Rolling average (7-day) of deaths by region")
g1 + aes(y = mortality_rate)+labs(title = "Mortality rate by region")

cat("\n## Tests\n")

g1 +aes(y = tested_roll_7)+ labs(title = "Rolling average (7-day) of tests by region")
g1 +aes(y = tested_roll_7)+ facet_wrap(~region, scale = "free")+ labs(title = "Rolling average (7-day) of tests by region")
g1 + aes(y = testing_rate)

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




