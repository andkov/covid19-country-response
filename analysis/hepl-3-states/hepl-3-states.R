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
prints_folder <- paste0("./analysis/hepl-3-states/prints/", strftime(Sys.Date()))
if(!file.exists(prints_folder)){
  dir.create(file.path(prints_folder))
}

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
  ,"Northeast" = "#7570B3"
  ,"Midwest" = "#D95F02"
)

state_colors <- c(
  "Florida" = "#E7298A"
  ,"New York" = "#7570B3"
  ,"Wisconsin" = "#D95F02"
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
    path     = prints_folder,
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

# ds_events <- readr::read_csv("./analysis/us-response-2/key-dates-short.csv")
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
  mutate(
    region = fct_recode(region, "Midwest" = "MidWest", "Northeast" = "NorthEast")
  ) %>%
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
  arrange(area,date) %>%
  mutate(
    event_n = dplyr::row_number()
  ) %>%
  ungroup()

ds_events %>% glimpse()
# ----- g1 --------
focus_metrics <- c(
  "Cases (7-day average)"
  ,"Deaths (7-day average)"
  # ,"Tests (7-day average)"
)


d <- ds_jh_state %>%
  compute_epi(c("date", "country"), long = T) %>%
  filter(metric %in% focus_metrics) %>%
  filter(date <= as.Date("2020-11-25")) %>%
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
       subtitle = "7-day Running Average"
       ,x = "2020", y = NULL,
       caption = "(1) - Voluntary national shutdown instituted, limiting mass gatherings to 10 or less people, advising against discretionary travel,\n and recommending closure of schools, restaurants, gyms, and other indoor or outdoor venues. Extended through April 30.\n(2) - The White House released guidelines for state governors and local authorities to reopen the country.\nSource: JHU CSSE COVID-19 Data (https://github.com/CSSEGISandData/COVID-19)")+
  theme(
    text = element_text(size = 8 )
    ,strip.text = element_text(size = 8)
    ,plot.title = element_text(vjust =-3)
    ,plot.subtitle = element_text(vjust = -1)
    ,plot.caption = element_text(size = 5.5)
  )
g %>% quick_save2("01-us-cases-deaths", width = 120, height = 70, units = "mm")



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

latest_date <- "2020-11-25"
latest_date_test <- "2020-11-09"

# Region level
d <- ds_jh_state %>%
  compute_epi(c("date", "region", "country"), long = T) %>%
  filter(metric %in% focus_metrics) %>%
  mutate(
    metric = fct_relevel(metric, focus_metrics) %>% fct_drop()
  ) %>%
  filter(
    (date < as.Date(latest_date) & date > as.Date("2020-03-01") & metric %in% c("Cases (7DA/100K)","Cases (cum/100K)") )
    |
      (date < as.Date(latest_date) & date > as.Date("2020-03-01") & metric %in% c("Deaths (7DA/100K)","Deaths (cum/100K)") )
    |
      (date < as.Date(latest_date_test) & date > as.Date("2020-03-01") & metric %in% c("Tests (7DA/100K)","Tests (cum/100K)") )

  ) %>%
  filter(
    !(metric %in% c("Tests (7DA/100K)","Tests (cum/100K)" ) & date < as.Date("2020-04-19"))
    )


d$metric %>% levels()
g <-  d %>%
  ggplot(aes(x=date, y = value, group = region, color=region))+
  geom_line(size=2, alpha = .3)+
  geom_line(size = .4, alpha = 1)+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  facet_wrap(~metric, scales = "free", ncol = 3)+
  scale_color_manual(values = region_colors)+
  guides(color = guide_legend(nrow = 1))+
  labs(title = "COVID-19 Cases, Deaths, and Tests by US regions",
       subtitle = "7-day average (7DA) and Cumulative (cum) counts per 100,000 of population (100K)"
       ,x = "2020", y = NULL, color = "                                   Region"
       ,caption = "Source: JHU CSSE COVID-19 Data (https://github.com/CSSEGISandData/COVID-19)")+
  theme(
    legend.position = c(0.82, 1.125)
    , legend.background = element_rect(colour=NA, fill=NA)
    , legend.spacing.y =unit(.05, 'cm')
    ,text = element_text(size = 8)
   )

# g %>% quick_save("04-allmetrics-by-region", width = 2400, height = 1350, res = 200)
# g %>% quick_save2("02-covid-by-regions", width = 10, height = 6, units = "in")
g %>% quick_save2("02-covid-by-regions", width = 190, height = 120, units = "mm")
# ---- g2-monochrome -------
# metric_order
focus_metrics <- c(
  "Cases (7DA/100K)"
  ,"Deaths (7DA/100K)"
  ,"Tests (7DA/100K)"
  ,"Cases (cum/100K)"
  ,"Deaths (cum/100K)"
  ,"Tests (cum/100K)"
)

latest_date <- "2020-11-25"
latest_date_test <- "2020-11-09"

# Region level
d <- ds_jh_state %>%
  compute_epi(c("date", "region", "country"), long = T) %>%
  filter(metric %in% focus_metrics) %>%
  mutate(
    metric = fct_relevel(metric, focus_metrics) %>% fct_drop()
  ) %>%
  filter(
    (date < as.Date(latest_date) & date > as.Date("2020-03-01") & metric %in% c("Cases (7DA/100K)","Cases (cum/100K)") )
    |
      (date < as.Date(latest_date) & date > as.Date("2020-03-01") & metric %in% c("Deaths (7DA/100K)","Deaths (cum/100K)") )
    |
      (date < as.Date(latest_date_test) & date > as.Date("2020-03-01") & metric %in% c("Tests (7DA/100K)","Tests (cum/100K)") )

  ) %>%
  filter(
    !(metric %in% c("Tests (7DA/100K)","Tests (cum/100K)" ) & date < as.Date("2020-04-19"))
    )

# region_linetype <- c("South" = "dotted", "West" = "dotdash", "Northeast" = "solid", "Midwest" = "longdash")
# region_linetype <- c("South" = "dotted", "West" = "dashed", "Northeast" = "solid", "Midwest" = "1F")
region_linetype <- c("South" = "dotted", "West" = "dashed", "Northeast" = "solid", "Midwest" = "dotdash")

d$metric %>% levels()
g <-  d %>%
  # ggplot(aes(x=date, y = value, group = region, color=region, linetype=region))+
  ggplot(aes(x=date, y = value, group = region, linetype=region))+
  geom_line(size=1.3, alpha = .08, color = "black", linetype = "solid")+
  geom_line(size = .4, alpha = 1)+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  facet_wrap(~metric, scales = "free", ncol = 3)+
  scale_color_manual(values = region_colors)+
  scale_linetype_manual(values = region_linetype, name = "                                            Region")+
  guides(linetype = guide_legend(nrow=1))+
  # guides(color = guide_legend(nrow = 1))+
  labs(title = "COVID-19 Cases, Deaths, and Tests by US regions",
       subtitle = "7-day average (7DA) and Cumulative (cum) counts per 100,000 of population (100K)"
       ,x = "2020", y = NULL, color = "                                   Region"
       ,caption = "Source: JHU CSSE COVID-19 Data (https://github.com/CSSEGISandData/COVID-19)")+
  theme(
    legend.position = c(0.78, 1.125)
    , legend.background = element_rect(colour=NA, fill=NA)
    , legend.spacing.y =unit(-.85, 'cm')
    ,text = element_text(size = 8)
    ,legend.key.width = unit(10,"mm")
  )

# g %>% quick_save("04-allmetrics-by-region", width = 2400, height = 1350, res = 200)
# g %>% quick_save2("02-covid-by-regions", width = 10, height = 6, units = "in")
g %>% quick_save2("02-covid-by-regions-monochrome", width = 190, height = 120, units = "mm")

# ----- g3-------
focus_metrics <- c(
  "Cases (7DA/100K)"
  ,"Tests (7DA/100K)"
)

latest_date <- "2020-11-25"
latest_date_test <- "2020-10-26"

state_colors <- c("New York" = "#7570B3", "Florida" = "#E7298A", "Wisconsin" = "#D95F02" )

d <- ds_jh_state %>%
  compute_epi(c("date","state","region", "country"), long = T) %>%
  filter(metric %in% focus_metrics) %>%
  filter(state %in% c("New York","Florida","Wisconsin")) %>%
  filter(
    (date < as.Date(latest_date_test) & date > as.Date("2020-03-01") & metric == "Tests (7DA/100K)")
    |
    (date < as.Date(latest_date) & date > as.Date("2020-03-01") & metric ==  "Cases (7DA/100K)")

    ) %>%
  filter( !(metric %in% c("Tests (7DA/100K)","Tests (cum/100K)" ) & date < as.Date("2020-04-19"))) %>%
  mutate(
    metric = fct_drop(metric)
    ,value = case_when(metric == "Tests (7DA/100K)" ~ value/10, TRUE ~ value)
    ,metric = fct_recode(metric, "Tests (7DA/1m)" = "Tests (7DA/100K)")
    ,state_metric = paste0(state,metric))%>%
  left_join(
    ds_events, by = c("date" = "date", "state" = "area")
  ) %>%
  mutate(
    state = factor(state,levels = c("New York", "Florida", "Wisconsin"))
  )
# d %>% glimpse()
# d %>% group_by(metric) %>% summarize(n = n())
fontsize <- 2
g <-  d %>%
  ggplot(aes(x=date, y = value, group = state_metric, color = state, linetype = metric))+
  geom_line(alpha = .2, size = 3, linetype= "solid")+
  geom_line()+
  geom_point(shape = 19, size = 3,data = d %>% filter(!is.na(event_n)) %>% filter(metric == "Cases (7DA/100K)"))+
  geom_text(aes(label = event_n),size =2,data = d %>% filter(!is.na(event_n))%>% filter(metric == "Cases (7DA/100K)"),color ="white" )+
  # legend for New York
  geom_point(shape = 19, size = 3,data = d %>% filter(!is.na(event_n), state=="New York", metric == "Cases (7DA/100K)"), aes(y = seq(65,50,-5), x = as.Date("2020-03-01")) )+
  geom_text(aes(label = event_n,y = seq(65,50,-5), x = as.Date("2020-03-01")),size =2,data = d %>% filter(!is.na(event_n), state=="New York", metric == "Cases (7DA/100K)"),color ="white")+
  geom_text(aes(label = event_description_short,y = seq(65,50,-5), x = as.Date("2020-03-05")),size =fontsize,data = d %>% filter(!is.na(event_n), state=="New York", metric == "Cases (7DA/100K)"),color ="black",hjust=0)+
  # legend for Florida
  geom_point(shape = 19, size = 3,data = d %>% filter(!is.na(event_n), state=="Florida", metric == "Cases (7DA/100K)"), aes(y = seq(95,75,-5), x = as.Date("2020-04-30")) )+
  geom_text(aes(label = event_n,y = seq(95,75,-5), x = as.Date("2020-04-30")),size =2,data = d %>% filter(!is.na(event_n), state=="Florida", metric == "Cases (7DA/100K)"),color ="white")+
  geom_text(aes(label = event_description_short,y = seq(95,75,-5), x = as.Date("2020-05-04")),size =fontsize,data = d %>% filter(!is.na(event_n), state=="Florida", metric == "Cases (7DA/100K)"),color ="black",hjust=0)+
  # legend for Wisconsin
  geom_point(shape = 19, size = 3,data = d %>% filter(!is.na(event_n), state=="Wisconsin", metric == "Cases (7DA/100K)"), aes(y = seq(125,95,-5), x = as.Date("2020-07-28")) )+
  geom_text(aes(label = event_n,y = seq(125,95,-5), x = as.Date("2020-07-28")),size =2,data = d %>% filter(!is.na(event_n), state=="Wisconsin", metric == "Cases (7DA/100K)"),color ="white")+
  geom_text(aes(label = event_description_short,y = seq(125,95,-5), x = as.Date("2020-08-02")),size =fontsize,data = d %>% filter(!is.na(event_n), state=="Wisconsin", metric == "Cases (7DA/100K)"),color ="black",hjust=0)+


  scale_linetype_manual(values = c("Cases (7DA/100K)"="solid","Tests (7DA/1m)" = "dashed"))+
  # facet_wrap(~metric, scales = "free_y", ncol = 1)+
  # lemon::facet_rep_wrap(~metric,scales = "free_y", ncol = 1,repeat.tick.labels = TRUE)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  scale_y_continuous(breaks = seq(0,120,20),labels = scales::comma_format(),sec.axis = sec_axis(~ . * 1,breaks = seq(10,130,20) ))+
  scale_color_manual(values = state_colors)+
  labs(
    title = "COVID-19 in New York, Florida, and Wisconsin: Confirmed Cases and Reported Tests",

       x = "2020", y = NULL, color = NULL, linetype=NULL
       ,caption = "Source: JHU CSSE COVID-19 Data (https://github.com/CSSEGISandData/COVID-19)"
       )+
  guides(color = guide_legend(nrow = 1))+
  guides(linetype = guide_legend(nrow = 1))+
  theme(
    # legend.position = c(0.11, 0.80)
    legend.position = c(0.25, 0.90)
    ,legend.background = element_rect(colour=NA, fill=NA)
    , legend.box.just = "bottom"
    , legend.spacing.y =unit(-.2, 'cm')
    , text = element_text(size = 8)
  )
g %>% quick_save2("03-NY-FL-WI", width = 190, height = 150, units = "mm")
# ----- g3-monochrome -------
focus_metrics <- c(
  "Cases (7DA/100K)"
  ,"Tests (7DA/100K)"
)

latest_date <- "2020-11-25"
latest_date_test <- "2020-10-26"


d <- ds_jh_state %>%
  compute_epi(c("date","state","region", "country"), long = T) %>%
  filter(metric %in% focus_metrics) %>%
  filter(state %in% c("New York","Florida","Wisconsin")) %>%
  filter(
    (date < as.Date(latest_date_test) & date > as.Date("2020-03-01") & metric == "Tests (7DA/100K)")
    |
    (date < as.Date(latest_date) & date > as.Date("2020-03-01") & metric ==  "Cases (7DA/100K)")

    ) %>%
  filter( !(metric %in% c("Tests (7DA/100K)","Tests (cum/100K)" ) & date < as.Date("2020-04-19"))) %>%
  mutate(
    metric = fct_drop(metric)
    # ,value = case_when(metric == "Tests (7DA/100K)" ~ value/10, TRUE ~ value)
    # ,metric = fct_recode(metric, "Tests (7DA/1m)" = "Tests (7DA/100K)")
    ,state_metric = paste0(state,metric))%>%
  left_join(
    ds_events, by = c("date" = "date", "state" = "area")
  ) %>%
  mutate(
    state = factor(state,levels = c("New York", "Florida", "Wisconsin"))
  )
# d %>% glimpse()
# d %>% group_by(metric) %>% summarize(n = n())
# state_colors <- c("New York" = "#7570B3", "Florida" = "#E7298A", "Wisconsin" = "#D95F02" )
# state_colors <- c("New York" = "#7570B3", "Florida" = "#E7298A", "Wisconsin" = "#D95F02" )
metric_colors <- c("Cases (7DA/100K)" = "grey80","Tests (7DA/1m)" = "grey20" )
state_shapes <- c("New York" = 21, "Florida" = 22, "Wisconsin" = 23 )
# state_linetypes <- c("New York" = "dotted", "Florida" = "dotdash", "Wisconsin" ="dashed" )
state_linetypes <- c("New York" = "solid", "Florida" = "dotted", "Wisconsin" ="dashed" )


# region_linetype <- c("South" = "dotted", "West" = "dotdash", "Northeast" = "solid", "Midwest" = "longdash")


fontsize <- 2
g1 <-  d %>%
  filter(metric == "Cases (7DA/100K)") %>%
  ggplot(aes(x=date, y = value, group = state_metric, linetype = state))+
  # geom_line(alpha = .2, size = 3, linetype= "solid")+
  # geom_line(alpha = .2, size = 3)+
  geom_line()+
  # geom_point(aes(shape = state),size = 3,data = d %>% filter(!is.na(event_n)) %>% filter(metric == "Cases (7DA/100K)"))+
  geom_point(aes(shape = state), size = 4,data = d %>% filter(!is.na(event_n)) %>% filter(metric == "Cases (7DA/100K)"), fill = "white", alpha = .5)+
  geom_text(aes(label = event_n),size =2,data = d %>% filter(!is.na(event_n))%>% filter(metric == "Cases (7DA/100K)"),color ="black" )+
  # legend for New York
  geom_point(shape = 21, size = 3,data = d %>% filter(!is.na(event_n), state=="New York", metric == "Cases (7DA/100K)"), aes(y = seq(65,50,-5), x = as.Date("2020-03-01")) )+
  geom_text(aes(label = event_n,y = seq(65,50,-5), x = as.Date("2020-03-01")),size =2,data = d %>% filter(!is.na(event_n), state=="New York", metric == "Cases (7DA/100K)"),color ="black")+
  geom_text(aes(label = event_description_short,y = seq(65,50,-5), x = as.Date("2020-03-05")),size =fontsize,data = d %>% filter(!is.na(event_n), state=="New York", metric == "Cases (7DA/100K)"),color ="black",hjust=0)+
  # legend for Florida
  geom_point(shape = 22, size = 3,data = d %>% filter(!is.na(event_n), state=="Florida", metric == "Cases (7DA/100K)"), aes(y = seq(95,75,-5), x = as.Date("2020-04-30")) )+
  geom_text(aes(label = event_n,y = seq(95,75,-5), x = as.Date("2020-04-30")),size =2,data = d %>% filter(!is.na(event_n), state=="Florida", metric == "Cases (7DA/100K)"),color ="black")+
  geom_text(aes(label = event_description_short,y = seq(95,75,-5), x = as.Date("2020-05-04")),size =fontsize,data = d %>% filter(!is.na(event_n), state=="Florida", metric == "Cases (7DA/100K)"),color ="black",hjust=0)+
  # legend for Wisconsin
  geom_point(shape = 23, size = 3,data = d %>% filter(!is.na(event_n), state=="Wisconsin", metric == "Cases (7DA/100K)"), aes(y = seq(125,95,-5), x = as.Date("2020-07-28")) )+
  geom_text(aes(label = event_n,y = seq(125,95,-5), x = as.Date("2020-07-28")),size =2,data = d %>% filter(!is.na(event_n), state=="Wisconsin", metric == "Cases (7DA/100K)"),color ="black")+
  geom_text(aes(label = event_description_short,y = seq(125,95,-5), x = as.Date("2020-08-02")),size =fontsize,data = d %>% filter(!is.na(event_n), state=="Wisconsin", metric == "Cases (7DA/100K)"),color ="black",hjust=0)+


  # scale_linetype_manual(values = c("Cases (7DA/100K)"="solid","Tests (7DA/1m)" = "dashed"))+
  # facet_wrap(~metric, scales = "free_y", ncol = 1)+
  # lemon::facet_rep_wrap(~metric,scales = "free_y", ncol = 1,repeat.tick.labels = TRUE)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = as.Date(c("2020-03-01", "2020-11-25")) )+
  scale_y_continuous(breaks = seq(0,120,20),labels = scales::comma_format(),sec.axis = sec_axis(~ . * 1,breaks = seq(10,130,20) ))+
  # scale_color_manual(values = metric_colors)+
  scale_shape_manual(values = state_shapes      , name = "                         State")+
  scale_linetype_manual(values = state_linetypes, name = "                         State")+
  labs(
    title = "COVID-19 in New York, Florida, and Wisconsin: Confirmed Cases and Reported Tests",

       x = "    2020", y = "Cases (7DA/100K)", color = NULL, linetype=NULL
       # ,caption = "Source: JHU CSSE COVID-19 Data (https://github.com/CSSEGISandData/COVID-19)"
       )+
  # guides(color = guide_legend(nrow = 1))+
  guides(linetype = guide_legend(nrow = 1), shape = guide_legend(nrow =1))+
  # guides(shape = guide_legend(nrow = 1))+
  theme(
    # legend.position = c(0.11, 0.80)
    legend.position = c(0.25, 0.90)
    ,legend.background = element_rect(colour=NA, fill=NA)
    , legend.box.just = "bottom"
    , legend.spacing.y =unit(.15, 'cm')
    , text = element_text(size = 8)
    ,legend.key.width = unit(15,"mm")
    ,axis.text.x = element_text(vjust =-6.5)
  )
# g1 %>% quick_save2("03-NY-FL-WI-monochrome-top", width = 190, height = 120, units = "mm")


g2 <-  d %>%
  filter(metric == "Tests (7DA/100K)") %>%
  ggplot(aes(x=date, y = value, group = state_metric, linetype = state))+
  geom_line()+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = as.Date(c("2020-03-01", "2020-11-25")), position = "top" )+
  # scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(breaks = seq(0,600,100),labels = scales::comma_format(),sec.axis = sec_axis(~ . * 1,breaks = seq(50,650,100) ))+
  scale_linetype_manual(values = state_linetypes, name = "                                               State")+
  guides(linetype = guide_legend(nrow = 1), shape = guide_legend(nrow =1))+
  labs(y = "Tests (7DA/100K)"
       , x = NULL
       ,caption = "Source: JHU CSSE COVID-19 Data (https://github.com/CSSEGISandData/COVID-19)")+
  theme(
    # legend.position = c(0.11, 0.80)
    legend.position = c(0.25, 0.75)
    ,legend.background = element_rect(colour=NA, fill=NA)
    , legend.box.just = "bottom"
    , legend.spacing.y =unit(.15, 'cm')
    , text = element_text(size = 8)
    ,legend.key.width = unit(15,"mm")
    ,axis.text.x = element_blank()
  )

# g2 %>% quick_save2("03-NY-FL-WI-monochrome-bottom", width = 190, height = 50, units = "mm")


g3 <- cowplot::plot_grid(g1, g2, ncol =1, rel_heights = c(9,4))
g3 %>%  quick_save2("03-NY-FL-WI-monochrome", width = 190, height = 150, units = "mm")

# ---- fig1 ---------------------
file_paths <- c(
  "fig1" = paste0(prints_folder,"/01-us-cases-deaths",".jpg")
  ,"fig2" = paste0(prints_folder,"/02-covid-by-regions",".jpg")
  ,"fig3" = paste0(prints_folder,"/03-NY-FL-WI",".jpg")
)


file_paths["fig1"] %>% jpeg::readJPEG() %>% grid::grid.raster()

# ---- fig2 ---------------------
file_paths["fig2"] %>% jpeg::readJPEG() %>% grid::grid.raster()

# ---- fig3 ---------------------
file_paths["fig3"] %>% jpeg::readJPEG() %>% grid::grid.raster()

# ---- publish ---------------------------------------
path_report <- "./analysis/hepl-3-states/hepl-3-states.Rmd"
rmarkdown::render(
  input = path_report
  ,output_format=c("html_document")
  ,clean=TRUE
)





