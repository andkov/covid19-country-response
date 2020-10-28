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

# ---- declare-globals --------------------
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)

# ---- declare-functions ---------------------------

# ---- load-data -------------------------------------------------------------
ds_daily <- readr::read_csv(config$path_input_jh_daily)# state level, no county level!!
# ds_daily %>% glimpse()
ds_usts <- readr::read_csv(config$path_input_jh_usts) # timeseries
# Total US population estimate
ds_us_pop <- readr::read_rds("./data-public/derived/us-pop-estimate.rds")

ds_vote <- readr::read_rds("./data-public/derived/us-2020-state-political-results.rds")

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

ds_usts <- ds_usts %>%
  left_join(
    ds_us_pop %>% distinct(region, division,state_name)
    , by  = c("province_state" = "state_name")
  ) %>%
  mutate(
    incident_rate = n_cases/population*100000
    ,mortality_rate = n_deaths/population*100000
  ) %>%
  dplyr::mutate(
    region = dplyr::case_when(
      (province_state %in% varnames_crouise) ~ "Cruiseship",
      (province_state %in% varnames_territories) ~ "Territories",
      TRUE ~ as.character(region)
      ) %>% as_factor(),
    division = dplyr::case_when(
      province_state %in% varnames_crouise ~ "Cruiseship",
      province_state %in% varnames_territories ~ "Territories",
      TRUE ~ as.character(division)
    ) %>% as_factor()
  )

# ds_usts %>% glimpse()

# create a state-level version of usts for the purposes of joining with ds_daily dataframe
group_by_vars <- c("date", "province_state", "division", "region", "country_region")
summarize_vars <- c("n_cases","n_deaths","population")
ds_usts_state <- ds_usts %>%
  group_by(.dots = group_by_vars) %>%
  summarize_at(.vars = summarize_vars, .funs = sum, na.rm = T)
# add testing data from the ds_daily
# ds_daily %>% glimpse()
# ds_usts_state %>% glimpse()
ds_jh_state <- ds_usts_state %>%
  left_join(ds_daily %>% select(date,province_state, people_tested), by = c("date","province_state")) %>%   filter(!region %in% c("Territories", "Cruiseship") )


ds_covid_vote <- ds_jh_state %>%
  left_join(
    ds_vote %>% select(-c("state_po","state_fips")), by = "province_state"
  )


# ----- save-to-disk  -----------------------


ds_covid_vote %>% readr::write_rds("./data-unshared/derived/covid-vote.rds", compress = "gz")



# ------ exploration-script ------------------

# scrip below is meant purely for exploration of the created dataset

# ds_usts %>% arrange(region, division) %>%  distinct(province_state, division, region)  %>% View()
# some of the testing number exibit non-monotonic change, to expose the data:
# ds_daily_state %>% glimpse()
# For testing
g <- ds_daily %>%
  ggplot(aes(x = date , y = people_tested))+
  geom_line()+
  scale_y_continuous(label = scales::comma_format())+
  facet_wrap(~province_state, scales = "free")
# ggsave("./analysis//tests-by-state.jpg",g,device = "jpg",width = 18, height = 12, dpi = "retina" )

# -----inspect ---------------
#
# ds_jh_state %>% glimpse()

# -----  epi-function-1 ------------------
# computation function that works on merged file (ds_usts + ds_daily)

# NOTE: consider adding relative temporal anchors for time zero (e.g. days since first case)
compute_epi <- function(d, grouping_vars, var_cases = "n_cases", var_deaths = "n_deaths", var_tests = "people_tested", long =FALSE){
  # d <- ds_jh_state %>% filter(province_state == "Florida")
  # grouping_vars <- c("date","province_state")
  # grouping_vars <- c("date")
  # var_cases = "n_cases"
  # var_deaths = "n_deaths"
  # var_tests = "people_tested"
  # long = F
  # browser()
  grouping_vars_enquo <- rlang::syms(grouping_vars)
  grouping_vars_no_date <- rlang::syms(setdiff(grouping_vars,"date"))
  var_cases_enquo  <- rlang::sym(var_cases)
  var_deaths_enquo <- rlang::sym(var_deaths)
  var_tests_enquo  <- rlang::sym(var_tests)
  metric_order <- c(
    "n_cases_roll_7"   = "Cases (7-day average)"
    ,"n_cases_roll_7_rate" = "Cases(7DA/100K)"
    ,"n_cases_cum"      = "Cases (cumulative)"
    ,"incident_rate"    = "Cases (cum/100K)"

    ,"n_deaths_roll_7"  = "Deaths (7-day average)"
    ,"n_deaths_roll_7_rate"  = "Deaths (7DA/100K)"
    ,"n_deaths_cum"     = "Deaths (cumulative)"
    ,"mortality_rate"   = "Deaths (cum/100K)"

    ,"n_tests_roll_7"   = "Tests (7-day average)"
    ,"n_tests_roll_7_rate" = "Tests (7DA/100K)"
    ,"n_tests_cum"      = "Tests (cumulative)"
    ,"testing_rate"    = "Tests (cum/100K)"

  )

  d_out <- d %>%
    dplyr::arrange(!!!grouping_vars_enquo) %>%
    dplyr::group_by(!!!grouping_vars_enquo) %>%
    dplyr::summarize(
      n_cases_cum     = sum(!!var_cases_enquo, na.rm = T)
      ,n_deaths_cum   = sum(!!var_deaths_enquo, na.rm = T)
      ,n_tests_cum    = sum(!!var_tests_enquo, na.rm = T)
      ,population     = sum(population, na.rm = T)
      ,incident_rate  = n_cases_cum/population*100000
      ,mortality_rate = n_deaths_cum/population*100000
      ,testing_rate   = n_tests_cum/population*100000
      ,.groups = "keep"
    ) %>%
    group_by(!!!grouping_vars_no_date) %>%
    arrange(date) %>%
    dplyr::mutate(
      n_cases   = n_cases_cum - lag(n_cases_cum,1)
      ,n_deaths  = n_deaths_cum - lag(n_deaths_cum,1)
      ,n_tests    = n_tests_cum - lag(n_tests_cum,1)
      ,n_cases_roll_7 = zoo::rollapply(n_cases, 7, mean, align = 'right', fill = NA)
      ,n_deaths_roll_7 = zoo::rollapply(n_deaths, 7, mean, align = 'right', fill = NA)
      ,n_tests_roll_7 = zoo::rollapply(n_tests, 7, mean, align = 'right', fill = NA)
      ,n_cases_roll_7_rate = n_cases_roll_7/population*100000
      ,n_deaths_roll_7_rate = n_deaths_roll_7/population*100000
      ,n_tests_roll_7_rate = n_tests_roll_7/population*100000
    ) %>%
    ungroup() %>%
    select(all_of(c(grouping_vars,names(metric_order))))
  # d_out %>% glimpse()
  if(long){
    var_pivot_longer <- setdiff(names(d_out), grouping_vars)
    d_out <- d_out %>%
      tidyr::pivot_longer(cols = var_pivot_longer, names_to = "metric", values_to = "value") %>%
      mutate(
        metric = factor(metric, levels = names(metric_order), labels = metric_order)
      )
  }
  d_out <- d_out %>% dplyr::na_if(0L)
  return(d_out)
}
# Testing
#
#
quick_save <- function(g,name,...){
  ggplot2::ggsave(
    filename = paste0(name,".png"),
    plot     = g,
    device   = png,
    path     = "./analysis/covid-vote-1/prints/", # female marital educ poor_healt
    # width    = width,
    # height   = height,
    # units = "cm",
    # dpi      = 'retina',
    limitsize = FALSE,
    ...
  )
}

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
  # browser()
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

d <-  ds_covid_vote %>%
  compute_epi(c("date", "province_state","division", "region", "country_region", "winner_2016","governor_political_affiliation"), long = T)
gp <- d %>% filter(metric == "Deaths (cum/100K)")  %>% print_plotly_lines(measure = "value" )

# ------ graphing ----------
# State level
d <- ds_covid_vote %>%
  compute_epi(c("date", "province_state","division", "region", "country_region", "winner_2016","governor_political_affiliation"), long = T)
g <-  d %>%
  ggplot(aes(x=date, y = value, group = province_state, color = governor_political_affiliation))+
  geom_line(alpha = .2)+
  geom_smooth(aes(group = country_region))+
  scale_y_continuous(labels = scales::comma_format())+
  facet_wrap(~metric, scales = "free", ncol = 4)
g %>% quick_save("state_governor", width = 1600, height = 900, res = 120)

# Region level
d <- ds_jh_state %>%
  compute_epi(c("date", "region", "country_region"), long = T)
g <-  d %>%
  ggplot(aes(x=date, y = value, group = region, color=region))+
  geom_line(size=2, alpha = .5)+
  geom_line(size = .5, alpha = 1)+
  scale_color_viridis_d(option = "plasma", begin = .2, end = .9, name = "Region")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  facet_wrap(~metric, scales = "free", ncol = 4)
g %>% quick_save("region", width = 2400, height = 1350, res = 200)


ggsave()

# ----- ---------------------
d <- ds_jh_state %>%
  # filter(province_state == "Florida") %>%
  compute_epi(c("date","province_state", "division", "region"), long = F)
d %>% glimpse()

d %>%
  filter(division == "Territories") %>%
  # ggplot(aes(x = date, y = n_tests_roll_7))+
  # ggplot(aes(x = date, y = n_tests_roll_7_rate))+
  # ggplot(aes(x = date, y = n_tests_cum))+
  ggplot(aes(x = date, y = testing_rate))+
  geom_line()+
  facet_wrap(~province_state)

d <- ds_jh_state %>%
  compute_epi(c("date"), long = T)
d %>%
  ggplot(aes(x=date, y = value))+
  geom_line()+
  facet_wrap(~metric, scales = "free", ncol = 4)


# State
d <- ds_jh_state %>%
  compute_epi(c("date","province_state"), long = T)
d %>%
  ggplot(aes(x=date, y = value, group = province_state))+
  geom_line()+
  facet_wrap(~metric, scales = "free", ncol = 4)

# division
d <- ds_jh_state %>%
  compute_epi(c("date","division"), long = T)
d %>%
  ggplot(aes(x=date, y = value, group = division))+
  geom_line()+
  facet_wrap(~metric, scales = "free", ncol = 4)

# division + region
d <- ds_jh_state %>%
  compute_epi(c("date","division","region"), long = T)
d %>%
  ggplot(aes(x=date, y = value, group = division, color = region))+
  geom_line()+
  facet_wrap(~metric, scales = "free", ncol = 4)

# create state-level data frame
ds_usts_state <- ds_usts %>%
  compute_epi(grouping_vars = c("date","province_state","division","region" ))
ds_usts_state %>% glimpse()



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




