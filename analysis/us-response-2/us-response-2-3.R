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

division_levels <- c(
  "Pacific"
  ,"Mountain"
  ,"New England"
  ,"West North Central"
  ,"East North Central"
  ,"Middle Atlantic"
  ,"West South Central"
  ,"East South Central"
  ,"South Atlantic"
)

division_levels_display <- c(
"Pacific\nAK,CA,HI,OR,WA"
,"Mountain\nAZ,CO,ID,MT,NV,NM,UT,WY"
,"New England\nCT,ME,MA,NH,RI,VT"
,"West North Central\nIA,KS,MN,MO,NE,ND,SD"
,"East North Central\nIL,IN,MI,OH,WI"
,"Middle Atlantic\nNJ,NY,PA"
,"West South Central\nAR,LA,OK,TX"
,"East South Central\nAL,KY,MS,TN"
,"South Atlantic\nDE,DC,FL,GA,MD,NC,SC,VA,WV"
)

region_colors <- c(
  "West" = "#1B9E77"
  ,"South" = "#E7298A"
  ,"NorthEast" = "#7570B3"
  ,"MidWest" = "#D95F02"
)

# ---- declare-functions ---------------------------

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
    ,"n_cases_roll_7_rate" = "Cases (7DA/100K)"
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
    path     = "./analysis/us-response-2/prints-3/",
    # width    = width,
    # height   = height,
    # units = "cm",
    # dpi      = 'retina',
    limitsize = FALSE,
    ...
  )
}

quick_save2 <- function(g,name,...){
  ggplot2::ggsave(
    filename = paste0(name,".jpg"),
    plot     = g,
    device   = "jpg",
    path     = "./analysis/us-response-2/prints-3/",
    # width    = width,
    # height   = height,
    # units = "cm",
    dpi      = 'retina',
    limitsize = FALSE,
    ...
  )
}






# ---- load-data -------------------------------------------------------------
# Produced by `./manipulation/scribe-john-hopkins.R`
ds_jh_state <- readr::read_rds("./data-unshared/derived/john-hopkins-state.rds")

# Source: Harvard Datavers (presidential) + Kaiser Foundation (state parties)
# Produced by `./manipulation/ellis-us-election-results-2.R`
ds_vote <- readr::read_rds("./data-public/derived/us-2020-state-political-results.rds")
# Note: political leadership reflects the state of 2020

# ---- tweak-data --------------------
ds_jh_state <- ds_jh_state %>%
  mutate(
    division = factor(division, levels = division_levels)
  ) %>%
  group_by(division) %>%
  mutate(
    div_states = as.character(state_abb) %>% unique() %>% paste(collapse = ",")
    ,div_states = str_remove(div_states, ",NA$")
    ,division_display = paste0(as.character(division),"\n",div_states)
    ,division_display = factor(division_display, levels = division_levels_display)
  ) %>%
  ungroup() %>%
  select(-div_states)


ds_jh_state %>% glimpse()

ds_covid_vote <- ds_jh_state %>%
  left_join(
    ds_vote %>% select(-c("state_po","state_fips")), by = c("state"= "province_state")
  )
ds_covid_vote %>% glimpse()

# -----inspect ---------------

# ----- graph-1 ------------------
# some of the testing number exibit non-monotonic change, to expose the data:
# ds_daily_state %>% glimpse()
selected_states <- c("Minnesota", "North Carolina", "Rhode Island", "Texas", "Wyoming")
g <- ds_jh_state %>%
  ggplot(aes(x = date , y = n_tested))+
  geom_line()+
  geom_line(color = "red", size = 3, alpha = .3, data = ds_jh_state %>%  filter(state %in% selected_states))+
  scale_y_continuous(label = scales::comma_format())+
  facet_wrap(~state, scales = "free")+
  labs(title = "Cumulative number of tests conducted in each state",
       caption = "Note that some states exhibit non-monotone change (of cumulative counts);\n
       this likely indicate errors with data collection",
       y = "People tests (cumulative)", x = "Date")
g %>% quick_save2("01-tests-by-state",width = 18, height = 12)


g <- ds_jh_state %>%
  compute_epi(c("date", "division","region"), long = T) %>%
  filter(metric == "Tests (cumulative)") %>%
  ggplot(aes(x = date , y = value))+
  geom_line()+
  geom_line(aes(color = region), size = 3, alpha = .5)+
  scale_y_continuous(label = scales::comma_format())+
  facet_wrap(~division, scales = "free")+
  scale_color_manual(values = region_colors)+
  scale_x_date(date_breaks = "2 month", date_labels = "%b" )+
  labs(title = "Cumulative number of tests conducted in each division",
       color = "Region", y = "People tested (cumulative)", x = "Date")
g %>% quick_save2("02-tests-by-division",width = 9, height = 6)


# ------ graph-2 ----------
# State level
d <- ds_jh_state %>%
  compute_epi(c("date", "state","division", "region", "country"), long = T)
g <-  d %>%
  ggplot(aes(x=date, y = value, group = state))+
  geom_line(alpha = .2)+
  scale_y_continuous(labels = scales::comma_format())+
  facet_wrap(~metric, scales = "free", ncol = 4)+
  scale_x_date(date_breaks = "2 month", date_labels = "%b" )+
  labs(title = "Cases, Deaths, and Tests (rows) for individual states",
       x = "Date", y = NULL)
g %>% quick_save2("03-allmetrics-by-state", width = 18, height = 12)

# Region level
d <- ds_jh_state %>%
  compute_epi(c("date", "region", "country"), long = T)
g <-  d %>%
  ggplot(aes(x=date, y = value, group = region, color=region))+
  geom_line(size=2, alpha = .5)+
  geom_line(size = .5, alpha = 1)+
  scale_color_viridis_d(option = "plasma", begin = .2, end = .9, name = "Region")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  facet_wrap(~metric, scales = "free", ncol = 4)+
  scale_color_manual(values = config$region_colors)+
  labs(title = "Cases, Deaths, and Tests (rows) for census regions",
       x = "Date", y = NULL)
# g %>% quick_save("04-allmetrics-by-region", width = 2400, height = 1350, res = 200)
g %>% quick_save2("04-allmetrics-by-region", width = 18, height = 12)


# ----- graph-3-------
focus_metric <- c("Cases (7DA/100K)","Cases (cum/100K)","Deaths (7DA/100K)","Deaths (cum/100K)" )
all_metrics <- c(
 "Cases (7-day average)"
 ,"Cases (7DA/100K)"
 ,"Cases (cumulative)"
 ,"Cases (cum/100K)"
 ,"Deaths (7-day average)"
 ,"Deaths (7DA/100K)"
 ,"Deaths (cumulative)"
 ,"Deaths (cum/100K)"
 ,"Tests (7-day average)"
 ,"Tests (7DA/100K)"
 ,"Tests (cumulative)"
 ,"Tests (cum/100K)"
)

# By division
for(metric_i in focus_metric){
  d1 <- ds_jh_state %>%
    compute_epi(c("date", "division_display", "region"), long = T)
  d2 <- ds_jh_state %>%
    compute_epi(c("date","state", "state_abb", "division_display", "region"), long = T)
  d3 <- bind_rows(d2,d1)

  d <- d3 %>%
    filter(metric %in% metric_i) %>%
    mutate(division = division_display)

  g <- d %>%
    ggplot(aes(x = date , y = value))+
    geom_line(aes(group=state), data = d %>% filter(!is.na(state)),alpha = .3)+
    geom_line(aes(group=division),data = d %>% filter(is.na(state)))+
    geom_line(aes(group=division, color = region), size = 3, alpha = .5, data = d %>% filter(is.na(state)))+
    scale_y_continuous(label = scales::comma_format())+
    facet_wrap(~division)+
    # facet_wrap(~division, scales = "free")+
    scale_color_manual(values = region_colors)+
    scale_x_date(date_breaks = "2 month", date_labels = "%b" )+
    labs(title = paste0(metric_i, " in each division"),
         color = "Region", y = metric_i, x = "Date")
  g %>% quick_save2(paste0("by-division\\",janitor::make_clean_names(metric_i)),width = 9, height = 7)
}


# By region
for(metric_i in focus_metric){
  d1 <- ds_jh_state %>%
    compute_epi(c("date", "region"), long = T)
  d2 <- ds_jh_state %>%
    compute_epi(c("date","state", "region"), long = T)
  d3 <- bind_rows(d2,d1)

  d <- d3 %>%
    filter(metric %in% metric_i)
  g <- d %>%
    ggplot(aes(x = date , y = value))+
    geom_line(aes(group=state), data = d %>% filter(!is.na(state)),alpha = .3)+
    geom_line(aes(group=region),data = d %>% filter(is.na(state)))+
    geom_line(aes(group=region, color = region), size = 3, alpha = .5, data = d %>% filter(is.na(state)))+
    scale_y_continuous(label = scales::comma_format())+
    # facet_wrap(~region, scales = "free", ncol = 4)+
    facet_wrap(~region, ncol = 4)+
    scale_color_manual(values = region_colors)+
    scale_x_date(date_breaks = "2 month", date_labels = "%b" )+
    labs(title = paste0(metric_i, " in each division"),
         color = "Region", y = metric_i, x = "Date")
  g %>% quick_save2(paste0("by-region\\",janitor::make_clean_names(metric_i)),width = 14, height = 4)
}


# ---- publish ---------------------------------------
path_report <- "./analysis/us-response/us-response-2.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)





