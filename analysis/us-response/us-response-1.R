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
      ,persistent = FALSE
      ,defaultValues = default_region          # highlights in the beginning
    ) %>%
    plotly::layout(margin = list(l = 0, r = 0, b = 80, t = 30, pad = 0))
  # plotly::layout(margin = margings_for_plotly)
  g1p

}




# ds_daily %>% print_plotly_lines("confirmed", y  = "Confirmed Cases", title = "XXX")
# ds_daily %>% print_plotly_lines("active")
# ds_cgrt %>% print_plotly_lines("stringency_index",grouping = "region_code",  default_region = c("USA","GBR","IRL","CAN"))

# ---- load-data -------------------------------------------------------------
# reference table for geographic units
# ds_geo <- readr::read_csv("./data-public/metadata/world-geography.csv")
ds_geo <- readr::read_csv("../COVID-19/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv") %>% janitor::clean_names()
# ds_geo %>% glimpse()
ds_covid <- readr::read_csv(config$path_input_covid)
# ds_covid %>% glimpse()
ds_cgrt <-  readr::read_rds(config$path_input_cgrt)
# ds_cgrt %>% glimpse()
ds_daily <- readr::read_csv(config$path_input_jh_daily)
# ds_daily %>% glimpse()

# to keep it manageble during exploration
# ds_cgrt <- ds_cgrt %>%  select(country_code, date, StringencyIndex )
# ds_cgrt %>% glimpse()

# n_distinct(ds_cgrt$country_code)
# ds_covid$country_code %>% unique() %>% length()
# ds_cgrt$country_code %>% unique() %>% length()

# metadata to
cgrt_key <- readxl::read_xlsx("./data-public/metadata/cgrt/cgrt-key.xlsx", sheet = "key")

meta_cgrt <- function(item_id,field){
  # item_id = "h1"
  # field  = "field_enqlabel"
  field_enq <- rlang::sym(field)
  cgrt_key %>% filter(id == item_id) %>%
    pull(field_enq)
}
# meta_cgrt("h2","name")

# ---- tweak-data --------------------
cgrt_key %>% select(id, name,measurement)


ds_cgrt <- ds_cgrt %>%
  mutate(
    region_code = ifelse(is.na(region_code), country_code, region_code)
    ,region_name = ifelse(is.na(region_name), country_name, region_name)
  ) #%>%
  # filter(
  #   # country_code == "USA"
  #   country_code %in% c("USA","GBR", "IRL","CAN")
  # )

# ds_cgrt %>% filter(country_code == "USA") %>% View()

# recode levels of CGRT
for(i in names(cgrt_levels)){
  # qname <- "c1"
  qname <- i
  item_name <- meta_cgrt(qname, "name")
  item_levels <- cgrt_levels[[qname]]

  ds_cgrt <- ds_cgrt %>%
    mutate_at(
      .vars = item_name, .funs = factor, levels = names(item_levels), labels = item_levels
    )
}
# inspect
# ds_cgrt %>% group_by(c1_school_closing) %>% count()



# d %>% glimpse()
# ds_cgrt %>% glimpse()
ds_cgrt <- ds_cgrt %>%
  mutate(
    month    = lubridate::month(date)
    ,month   = factor(month, levels =1:12, labels = month.abb)
    ,month   = fct_rev(month)
    ,week    = lubridate::week(date)
    ,day     = lubridate::day(date)
    ,weekday = lubridate::wday(date)
    ,province_state = region_name
  )


# ds_cgrt %>% glimpse()
print_tile <- function(d, region, measure, relative_h = c(2,1)){
  # d <-  ds_cgrt
  # region = "USA"
  # measure = "h2"
  measure_str <-  meta_cgrt(measure,"name")
  measure_label <-  meta_cgrt(measure,"label")
  measure_enq <- rlang::sym(measure_str)
  main_title = paste0("(",toupper(region),") - ", measure_label)
  d1 <- d %>%
    filter(region_name == region) #%>%
    # select(date,region_name, !!measure_enq)
  # d1
  g <- d1 %>%
    ggplot(aes(x=day, y = month, fill = !!measure_enq))+
    geom_tile(color = "white")+
    scale_fill_viridis_d(option = "magma", begin = .0, end = .9,  direction = -1)+
    theme(
      panel.grid = element_blank()
      # ,legend.position = "right"
    )+
    labs(y = NULL, x = "Day of the month",
         title = main_title, fill = meta_cgrt(measure, "label"))

  g_legend <- ggpubr::get_legend(g) %>% ggpubr::as_ggplot()
  g <- cowplot::plot_grid(
    g +theme(legend.position = "none")
    , g_legend,ncol=1, rel_heights = relative_h
  )
 g
}
 ds_cgrt%>% print_tile("Ireland","c2")
 ds_cgrt%>% print_tile("United States","c2")
 ds_cgrt%>% print_tile("Canada","c2")
 ds_cgrt%>% print_tile("United Kingdom","c2")

# g1_legend <- ggpubr::get_legend(g1) %>% ggpubr::as_ggplot()

# ggpubr::as_ggplot(g1_legend)

cowplot::plot_grid(g1 +theme(legend.position = "none"), g1_legend,ncol=1, rel_heights = c(2,1))
# ----- containment -------------
ds_cgrt %>% print_tile("USA","c1")

# ---- health-policy -----------
ds_cgrt %>% print_tile("USA","h1")

ds_cgrt %>% print_tile("USA","h2")

ds_cgrt %>% print_tile("USA","h3")


# ---- econ-support --------
ds %>% print_tile("USA","e1")

# ---- confirmed -------------------------
ds_daily %>% print_plotly_lines("confirmed", y = "Confirmed Cases", title = "Timeline of confirmed cases by state")


ds_daily %>% print_plotly_lines("incident_rate", y = "Incident Rate", title = "Timeline of incident rate by state")

# ---- deaths -------------------------
ds_daily %>% print_plotly_lines("deaths", y = "Deaths", title = "Timeline of deaths by state")


ds_daily %>% print_plotly_lines("mortality_rate", y = "Mortality Rate", title = "Timeline of mortality rate by state")

# ---- tested -------------------------
ds_daily %>% print_plotly_lines("people_tested", y = "People Tested", title = "Timeline of people tested by state")


ds_daily %>% print_plotly_lines("testing_rate", y = "Testing Rate", title = "Timeline of testing rate by state")

# ---- hospitalized -------------------------
ds_daily %>% print_plotly_lines("people_hospitalized", y = "People Hospitalized", title = "Timeline of hospitalizations by state")


ds_daily %>% print_plotly_lines("hospitalization_rate", y = "Hospitalization Rate", title = "Timeline of hospitalization rate by state")


# ---- stringency ----------------------
ds_cgrt %>% print_plotly_lines("stringency_index", y = "Stringency Index", title = "Timeline of stringency index by state", default_region = "USA")

# ---- government ----------------------
ds_cgrt %>% print_plotly_lines("government_response_index", y = "Government Response Index", title = "Timeline of Gov Response index by state", default_region = "Florida")

# ---- containment ----------------------
ds_cgrt %>% print_plotly_lines("containment_health_index ", y = "Containment Health Index", title = "Timeline of Containment Health index by state", default_region = "USA")

# ---- economy ----------------------
ds_cgrt %>% print_plotly_lines("economic_support_index ", y = "Economic Support Index", title = "Timeline of Economic Support index by state", default_region = "USA")






# ---- publish ---------------------------------------
path_report <- "./analysis/us-response/us-response-1.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)






