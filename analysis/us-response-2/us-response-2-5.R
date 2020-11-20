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
library(gt) # tables
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
      strip.background = element_rect(fill="grey95", color = NA)
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
    path     = "./analysis/us-response-2/prints-4/",
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
    path     = "./analysis/us-response-2/prints-5/",
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
# ds_vote <- readr::read_rds("./data-public/derived/us-2020-state-political-results.rds")
ds_vote <- readr::read_rds("./data-public/derived/us-2020-state-pres-results.rds")
# Note: political leadership reflects the state of 2020

ds_vote %>% glimpse()
# ds_vote2 %>% glimpse()

ds_events <- readr::read_delim("analysis/us-response-2/key-dates.csv",
                               "|", escape_double = FALSE, trim_ws = TRUE)
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

# ds_covid_vote <- ds_jh_state %>%
#   left_join(
#     ds_vote %>% select(-c("state_po","state_fips")), by = c("state"= "province_state")
#   )
ds_covid_vote <- ds_jh_state %>%
  left_join(
    ds_vote %>% mutate_at("state", factor),  by = c("state")
  )
ds_covid_vote %>% glimpse()


ds_events <- ds_events %>%
  group_by(area) %>%
  mutate(
    event_n = dplyr::row_number()
  )

# -----inspect ---------------


# ----- g1 --------
focus_metrics <- c(
  "Cases (7-day average)"
  ,"Deaths (7-day average)"
  # ,"Tests (7-day average)"
)


d <- ds_jh_state %>%
  compute_epi(c("date", "country"), long = T) %>%
  filter(metric %in% focus_metrics) %>%
  mutate(
    metric = fct_recode(metric
                        ,"Cases" =  "Cases (7-day average)"
                        ,"Deaths"=  "Deaths (7-day average)"
                        # ,"Tests" = "Tests (7-day average)"
    ) %>% fct_drop()
  ) %>%
  left_join(
    ds_events, by = c("date" = "date", "country" = "area")
  )

g <-  d %>%
  ggplot(aes(x=date, y = value))+
  geom_line(alpha = .1, size = 3)+
  geom_line()+
  # scale_x_date(limits = c(as.Date("2020-03-01"), max(d$date)))+
  scale_y_continuous(labels = scales::comma_format())+
  facet_wrap(~metric, scales = "free", ncol = 4)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b",limits = c(as.Date("2020-03-01"), max(d$date)))+
  geom_point(shape = 19, size = 3,data = d %>% filter(!is.na(event_n)))+
  geom_text(aes(label = event_n),size =2,data = d %>% filter(!is.na(event_n)),color ="white" )+
  labs(title = "Confirmed Cases and Deaths from COVID-19 in the United States",
       subtitle = "7-day average",
       x = "2020", y = NULL)
g %>% quick_save2("01-us-cases-deaths", width = 10, height = 4)


# ---- g2 -------
# metric_order
focus_metrics <- c(
  "Cases (7DA/100K)"
  ,"Deaths (7DA/100K)"
  ,"Tests (7DA/100K)"
  ,"Cases (cum/100K)"
  ,"Deaths (cum/100K)"
  ,"Tests (cum/100K)"
)


# Region level
d <- ds_jh_state %>%
  compute_epi(c("date", "region", "country"), long = T) %>%
  filter(metric %in% focus_metrics) %>%
  mutate(
    metric = fct_relevel(metric, focus_metrics) %>% fct_drop()
  ) %>%
  filter(
    date >= as.Date("2020-03-01"), date <= as.Date("2020-11-08") # because gives negative numbers after this date
  )


d$metric %>% levels()
g <-  d %>%
  ggplot(aes(x=date, y = value, group = region, color=region))+
  geom_line(size=2, alpha = .5)+
  geom_line(size = .5, alpha = 1)+
  # scale_color_viridis_d(option = "plasma", begin = .2, end = .9, name = "Region")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  facet_wrap(~metric, scales = "free", ncol = 3)+
  scale_color_manual(values = config$region_colors)+
  labs(title = "COVID-19 Cases, Deaths, and Tests by US regions",
       subtitle = "7-day average (7DA) and Cumulative (cum) counts per 100,000 of population (100K)"
       ,x = "2020", y = NULL)
# g %>% quick_save("04-allmetrics-by-region", width = 2400, height = 1350, res = 200)
g %>% quick_save2("02-covid-by-regions", width = 10, height = 6)


# ----- g3-------
focus_metrics <- c(

  "Cases (7DA/100K)"
  ,"Cases (cum/100K)"
  ,"Deaths (7DA/100K)"
  ,"Deaths (cum/100K)"
  ,"Tests (7DA/100K)"
  ,"Tests (cum/100K)"
)


d <- ds_jh_state %>%
  compute_epi(c("date","state","region", "country"), long = T) %>%
  filter(metric %in% focus_metrics) %>%
  filter(state %in% c("New York","Florida","Wisconsin")) %>%
  filter(date < as.Date("2020-10-26") & date > as.Date("2020-03-01")) %>%
  filter( !(metric %in% c("Tests (7DA/100K)","Tests (cum/100K)" ) & date < as.Date("2020-04-19"))) %>%
  mutate(
    metric = fct_drop(metric)
  ) %>%
  left_join(
    ds_events, by = c("date" = "date", "state" = "area")
  )

g <-  d %>%
  ggplot(aes(x=date, y = value, group = state, color = state))+
  geom_line(alpha = .1, size = 3)+
  geom_line()+
  geom_point(shape = 19, size = 3,data = d %>% filter(!is.na(event_n)))+
  geom_text(aes(label = event_n),size =2,data = d %>% filter(!is.na(event_n)),color ="white" )+
  scale_y_continuous(labels = scales::comma_format())+
  # facet_wrap(~metric, scales = "free_y", ncol = 1)+
  lemon::facet_rep_wrap(~metric,scales = "free_y", ncol = 1,repeat.tick.labels = TRUE)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  scale_color_manual(values = c("New York" = "#7570B3", "Florida" = "#E7298A",
                                "Wisconsin" = "#D95F02" ))+
  labs(title = "COVID-19 Cases, Deaths, and Tests for 3 US States",
       subtitle = "7-day average (7DA) and Cumulative (cum) counts per 100,000 of population (100K)",
       x = "2020", y = NULL, color = NULL)+
  theme(legend.position = "top")
g %>% quick_save2("03-NY-FL-WI-2", width = 10, height = 15)




# ----- g4-------
focus_metrics <- c(
  "Cases (7DA/100K)"
  ,"Tests (7DA/100K)"
)


d <- ds_jh_state %>%
  compute_epi(c("date","state","region", "country"), long = T) %>%
  filter(metric %in% focus_metrics) %>%
  filter(state %in% c("New York","Florida","Wisconsin")) %>%
  filter(date < as.Date("2020-10-26") & date > as.Date("2020-03-01")) %>%
  filter( !(metric %in% c("Tests (7DA/100K)","Tests (cum/100K)" ) & date < as.Date("2020-04-19"))) %>%
  mutate(
    metric = fct_drop(metric)
    ,value = case_when(metric == "Tests (7DA/100K)" ~ value/10, TRUE ~ value)
    ,metric = fct_recode(metric, "Tests (7DA/1m)" = "Tests (7DA/100K)")
    ,state_metric = paste0(state,metric)
  )%>%
  left_join(
    ds_events, by = c("date" = "date", "state" = "area")
  )
d %>% group_by(metric) %>% summarize(n = n())

g <-  d %>%
  ggplot(aes(x=date, y = value, group = state_metric, color = state, linetype = metric))+
  geom_line(alpha = .1, size = 3, linetype= "solid")+
  geom_line()+
  geom_point(shape = 19, size = 3,data = d %>% filter(!is.na(event_n)) %>% filter(metric == "Cases (7DA/100K)"))+
  geom_text(aes(label = event_n),size =2,data = d %>% filter(!is.na(event_n))%>% filter(metric == "Cases (7DA/100K)"),color ="white" )+
  scale_y_continuous(labels = scales::comma_format())+
  scale_linetype_manual(values = c("Cases (7DA/100K)"="solid","Tests (7DA/1m)" = "dotted"))+
  # facet_wrap(~metric, scales = "free_y", ncol = 1)+
  # lemon::facet_rep_wrap(~metric,scales = "free_y", ncol = 1,repeat.tick.labels = TRUE)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  scale_y_continuous(breaks = seq(0,70,10))+
  scale_color_manual(values = c("New York" = "#7570B3", "Florida" = "#E7298A",
                                "Wisconsin" = "#D95F02" ))+
  labs(title = "COVID-19 Cases and Tests for 3 US States",
       x = "2020", y = NULL, color = NULL, linetype=NULL)+
  theme(legend.position = "top")
g %>% quick_save2("04-NY-FL-WI", width = 10, height = 6)

# ---- create-description-table ------------------------------------------------


table_d <- readr::read_csv("./analysis/us-response-2/key-dates-short.csv") %>%
  filter(area != "US") %>%
  select(-event_descriptions) %>%
  group_by(area) %>%
  mutate(
    event_n = dplyr::row_number()
  ) %>% ungroup() %>%
  relocate(event_n) %>%
  as_tibble()

table1 <- table_d %>%
  gt() %>%
  cols_label(
    event_n                  = "Event Num"
    ,area                    = "State"
    ,date                    = "Data"
    ,event_description_short = "Description"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#7570B3")
      ,cell_text(color = "white", weight = "bold")
      )
    ,locations = cells_body(
      columns = vars(event_n)
      ,rows = area == "New York"
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E7298A")
      ,cell_text(color = "white", weight = "bold")
    )
    ,locations = cells_body(
      columns = vars(event_n)
      ,rows = area == "Florida"
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#D95F02")
      ,cell_text(color = "white", weight = "bold")
    )
    ,locations = cells_body(
      columns = vars(event_n)
      ,rows = area == "Wisconsin"
    )
  )


table1


# ---- publish ---------------------------------------
path_report <- "./analysis/us-response/us-response-2.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)





