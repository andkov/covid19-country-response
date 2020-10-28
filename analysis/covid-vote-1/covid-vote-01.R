
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio
# ---- NOTES -------------------------------------------------------------------




# ---- load-packages -----------------------------------------------------------
library(tidyverse)

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
compute_epi <- function(
  d
  ,grouping_vars
  ,var_cases = "n_cases"
  ,var_deaths = "n_deaths"
  ,var_tests = "n_tested"
  ,long =FALSE){
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

quick_save <- function(g,name,...){
  ggplot2::ggsave(
    filename = paste0(name,".png"),
    plot     = g,
    device   = png,
    path     = "./analysis/covid-vote-1/prints/",
    # width    = width,
    # height   = height,
    # units = "cm",
    # dpi      = 'retina',
    limitsize = FALSE,
    ...
  )
}

# ---- load-data ---------------------------------------------------------------
# Produced by `./manipulation/scribe-john-hopkins.R`
ds_jh_state <- readr::read_rds("./data-unshared/derived/john-hopkins-state.rds")

# Source: Harvard Datavers (presidential) + Kaiser Foundation (state parties)
# Produced by `./manipulation/ellis-us-election-results-2.R`
ds_vote <- readr::read_rds("./data-public/derived/us-2020-state-political-results.rds")
# Note: political leadership reflects the state of 2020

ds_covid_vote <- ds_jh_state %>%
  left_join(
    ds_vote %>% select(-c("state_po","state_fips")), by = c("state"= "province_state")
  )
ds_covid_vote %>% glimpse()

# ---- tweak-data --------------------------------------------------------------

# ---- graphing ----------------------------------------------------------------

political_colors <- c(
  "Democrat"    = "#0015BC"
  ,"Divided"    = "#800080"
  ,"Republican" = "#E9141D"

  )


d <-  ds_covid_vote %>%
  compute_epi(
    c(
      "date"
      ,"state"
      ,"division"
      ,"region"
      ,"country"
      ,"state_leadership"), long = T)


g <-  d %>%
  ggplot(
    aes(
      x  = date
      ,y = value
      ,group = province_state
      ,color = state_leadership
      )
    )+
  geom_line(alpha = .2)+
  # geom_smooth(aes(group = country_region))+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_manual(values = political_colors) +
  facet_wrap(~metric, scales = "free", ncol = 4)
g %>% quick_save("state_leadership", width = 1600, height = 900, res = 120)














# ---- publish ---------------------------------------

path_report <- "./analysis/covid-vote-1/covid-vote-01.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)




