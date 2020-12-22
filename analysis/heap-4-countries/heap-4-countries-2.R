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

prints_folder <- paste0("./analysis/heap-4-countries/", strftime(Sys.Date()),"/")
if(!file.exists(prints_folder)){
  dir.create(file.path(prints_folder))
}
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

# ---- inspect-data ----------------
# before applying compute_epi_timeline() we want to make sure
# that recorded numbers ARE NOT cumulative
ds_covid %>%
  filter(country_code == "USA") %>%
  ggplot(aes(x = date, y = n_cases))+
  geom_line()

ds_who %>%
  filter(country_code == "USA") %>%
  ggplot(aes(x = date, y = n_cases))+
  geom_line()

# ---- tweak-data --------------------

# focus_4_countries <- c("USA","GBR", "IRL","CAN")
focus_4_countries <- c("BEL", "CAN", "DEN", "DEU", "ESP", "FRA", "GBR", "IRL", "ITA", "NOR", "USA")
# ds_covid %>% select(country_code) %>%
#   left_join(ds_geo %>% select(country_code, country_name) ) %>%
#   distinct() %>% arrange(country_name) %>% View()

# prepare covid timelines from ECDC source
ds_covid <- ds_covid %>%
  rename(n_population = n_population_2018) %>%
  filter( country_code %in% focus_4_countries ) %>% # to make lighter
  left_join(
    ds_geo %>% select(country_code, country_label)
    ,by = "country_code"
  ) %>%
  select(date, country_code, country_label, everything())
ds_covid %>% glimpse()

ds_epi <- ds_covid %>%
  compute_epi_timeline() %>%
  select(-n_cases, -n_deaths)
# ds_epi %>% glimpse()

# prepare covid timelines from WHO source
ds_who <- ds_who %>%
  filter( country_code %in% focus_4_countries) %>% # to make lighter
  select(date,country_code, country_label, n_cases, n_deaths, n_population = population)
# ds_who %>% glimpse()

ds_epi_who <- ds_who %>%
  compute_epi_timeline() %>%
  select(-n_cases, -n_deaths)
# ds_epi_who %>% glimpse()
# combine
ls_epi <- list(
  "ecdc" = ds_epi, "who" = ds_epi_who
)
rm(ds_epi_who)
ds_epi <- ls_epi %>% bind_rows(.id = "source")
ds_epi %>% glimpse()

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
  # filter(source == "ecdc") %>%
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

ds2 <- ds1

ds1 <- ds1 %>%
  filter(country_code %in% c("USA","GBR", "IRL","CAN") )

# ds1a %>% distinct(country_code)
# ds1 <- ds1a %>%
#   filter(country_code %in% c("USA","GBR", "IRL","CAN"))
#
#
# ds2 <- ds1 %>%
#   filter(metric %in% c("Cases (cum/1M)", "Deaths (cum/1M)"))
# ds2 %>% glimpse()
#
#
# ds1_who <- ds_epi %>%
#   filter(source == "who") %>%
#   tidyr::pivot_longer(cols = names(metric_order), names_to = "metric", values_to = "value") %>%
#   mutate(
#     metric = factor(metric, levels = names(metric_order), labels = metric_order)
#   ) %>%
#   left_join(
#     ds_geo %>% distinct(country_code, country_label)
#   ) %>%
#   left_join(
#     ds_lockdown, by = c("date","country_code")
#   ) %>%
#   mutate_at(
#     .vars = c("lockdown_type","lockdown_label")
#     ,.funs = factor
#   )
# ds1_who %>% glimpse()
# ds1_who %>% distinct(country_label)


# ------ facet-graphs ----------------------
# see https://personal.sron.nl/~pault/#sec:qualitative
pal_bright <- c(
  "blue" = "#4477AA"
  ,"cyan" = "#66CCEE"
  ,"green" = "#228833"
  ,"yellow" = "#CCBB44"
  ,"red" = "#EE6677"
  ,"purple" = "#AA3377"
  ,"grey" = "#BBBBBB"
)

country_colors <- c(
  "Ireland"         = pal_bright[["green"]]
  ,"United Kingdom" = pal_bright[["blue"]]
  ,"United States"  = pal_bright[["yellow"]]
  ,"Canada"         = pal_bright[["purple"]]
)
country_linetype <- c(
  "Ireland"         = "longdash"
  ,"United Kingdom" = "dotdash"
  ,"United States"  = "solid"
  ,"Canada"         = "dotted"
)

# ------ singular -----------------------



d4 <- ds1 %>%
  filter(source == "ecdc") %>%
  filter(metric %in% c("Cases (7DA/1M)")) %>%
  filter(date >= as.Date("2020-03-01"))
scale_label <- "                                                           Country"
d4_legend <- d4 %>% distinct(letter_code, lockdown_label) %>% na.omit() %>%
  mutate(
    lockdown_label = fct_relevel(lockdown_label,
                                 "Voluntary lockdown or stay home request",
                                 "Mandatory national lockdown",
                                 "Country begins reopening"),
    letter_code = fct_relevel(letter_code, "V","M","R")
    ) %>%
  mutate(
    country_label = NA
  )
yseq <- seq(200,260,30)
xpos <- as.Date("2020-04-25")
g4 <- d4 %>%
  ggplot(aes_string(x="date", y = "value" ,group = "country_label", color = "country_label",linetype = "country_label"))+
  # geom_line(size = .5, color = "grey40")+
  geom_line(size = 3, alpha = .6, linetype = "solid")+ # think color
  geom_line(size = .5, alpha = 1, color = "black")+ # thin black
  geom_point(shape = 21,size =5, data = d4 %>% filter(!is.na(lockdown_type)),alpha=.9,aes(fill=country_label), color = "black", show.legend = FALSE)+
  geom_text(aes(label = letter_code), size = 3, color = "black")+
  # inset text legend
  geom_point(aes(x = xpos,y = yseq), shape = 21,size =5,color = "black",data = d4_legend)+
  geom_text(aes(label = letter_code, x = xpos,y = yseq), color = "black", size = 3,data = d4_legend)+
  geom_text(aes(label = paste0(" - ",lockdown_label), x = xpos+3,y = yseq), color = "black", size = 3,data = d4_legend , hjust=0)+
  # rest of the plot
  scale_linetype_manual(values = country_linetype, name = scale_label)+
  scale_color_manual(values = country_colors, name = scale_label)+
  scale_fill_manual(values = country_colors, name = scale_label)+
  scale_y_continuous(
    labels = scales::comma_format()
    , breaks = seq(0,550, 100)
    , minor_breaks = seq(0,550, 20)
    ,sec.axis = sec_axis(~ . * 1,breaks = seq(50,550, 100))
    )+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" ,
               expand = expansion(mult = c(0,.01)))+
  guides(fill = guide_legend(nrow = 1))+
  guides(color = guide_legend(nrow = 1))+
  guides(linetype = guide_legend(nrow = 1))+

  labs(y = "Cases (7-day average, per 1 million)", x = "2020", title = "Confirmed cases of COVID-19", subtitle = "7-day rolling average per 1 million of population", caption = "Source: European Centre for Disease Prevention and Control")+
  theme(
     legend.position = c(0.70, 1.05)
    ,legend.background = element_rect(colour=NA, fill=NA)
    ,legend.box.just = "bottom"
    ,legend.spacing.y =unit(.2, 'cm')
    ,text = element_text(size = 10)
    ,plot.caption = element_text(vjust = 5)
    # ,plot.title = element_text(vjust = - 15, hjust = .01)
    # ,plot.subtitle = element_text(vjust = - 19, hjust = .01)
    ,legend.key.width = unit(15,"mm")

  )
# g4a
ggsave(paste0(prints_folder,"cases_7da_per1m.jpg"),g4,"jpg", width = 10, height = 6,dpi = "retina")




d5 <- ds1 %>%
  filter(source == "ecdc") %>%
  filter(metric %in% c("Deaths (7DA/1M)")) %>%
  filter(date >= as.Date("2020-03-01"))
scale_label <- "                                                           Country"
d5_legend <- d4 %>% distinct(letter_code, lockdown_label) %>% na.omit() %>%
  mutate(
    lockdown_label = fct_relevel(lockdown_label,
                                 "Voluntary lockdown or stay home request",
                                 "Mandatory national lockdown",
                                 "Country begins reopening"),
    letter_code = fct_relevel(letter_code, "V","M","R")
  ) %>%
  mutate(
    country_label = NA
  )
yseq <- seq(7,8.4,.7)
xpos <- as.Date("2020-05-25")
g5 <- d5 %>%
  ggplot(aes_string(x="date", y = "value" ,group = "country_label", color = "country_label",linetype = "country_label"))+
  # geom_line(size = .5, color = "grey40")+
  geom_line(size = 3, alpha = .6, linetype = "solid")+ # think color
  geom_line(size = .5, alpha = 1, color = "black")+ # thin black
  geom_point(shape = 21,size =5, data = d5 %>% filter(!is.na(lockdown_type)),alpha=.9,aes(fill=country_label), color = "black", show.legend = FALSE)+
  geom_text(aes(label = letter_code), size = 3, color = "black")+
  # inset text legend
  geom_point(aes(x = xpos,y = yseq), shape = 21,size =5,color = "black",data = d4_legend)+
  geom_text(aes(label = letter_code, x = xpos,y = yseq), color = "black", size = 3,data = d4_legend)+
  geom_text(aes(label = paste0(" - ",lockdown_label), x = xpos+3,y = yseq), color = "black", size = 3,data = d4_legend , hjust=0)+
  # rest of the plot
  scale_linetype_manual(values = country_linetype, name = scale_label)+
  scale_color_manual(values = country_colors, name = scale_label)+
  scale_fill_manual(values = country_colors, name = scale_label)+
  scale_y_continuous(
    labels = scales::number_format(accuracy = 1)
    , breaks = seq(0,16,2), minor_breaks = seq(0,16, .5)
    ,sec.axis = sec_axis(~ . * 1,breaks = seq(1,16,2) )
  )+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" ,
               expand = expansion(mult = c(0,.01)))+
  guides(fill = guide_legend(nrow = 1))+
  guides(color = guide_legend(nrow = 1))+
  guides(linetype = guide_legend(nrow = 1))+

  labs(y = "Deaths (7-day average, per 1 million)", x = "2020", title = "Deaths from COVID-19", subtitle = "7-day rolling average per 1 million of population", caption = "Source: European Centre for Disease Prevention and Control")+
  theme(
    legend.position = c(0.70, 1.05)
    ,legend.background = element_rect(colour=NA, fill=NA)
    ,legend.box.just = "bottom"
    ,legend.spacing.y =unit(.2, 'cm')
    ,text = element_text(size = 10)
    ,plot.caption = element_text(vjust = 5)
    # ,plot.title = element_text(vjust = - 15, hjust = .01)
    # ,plot.subtitle = element_text(vjust = - 19, hjust = .01)
    ,legend.key.width = unit(15,"mm")

  )
# g4a
ggsave(paste0(prints_folder,"death_7da_per1m.jpg"),g5,"jpg", width = 10, height = 6,dpi = "retina")












d5 <- ds1 %>%
  filter(source == "ecdc") %>%
  filter(metric %in% c("Deaths (7DA/1M)")) %>%
  filter(date >= as.Date("2020-03-01"))

g5 <- d5 %>%
  ggplot(aes_string(x="date", y = "value", group = "country_label",color = "country_label",linetype = "country_label"))+
  geom_line(size = .5, color = "grey40")+
  geom_line(size = .5, alpha = .8)+
  geom_line(size = 3, alpha = .3, linetype = "solid")+
  geom_point(shape = 21,size =4, data = d5 %>% filter(!is.na(lockdown_type)) )+
  geom_text(aes(label = letter_code), size = 3, color = "black")+
  # scale_color_viridis_d(option = "plasma", begin = .2, end = .9, name = "Country")+
  scale_color_manual(values = country_colors, name = "Country")+
  scale_linetype_manual(values = country_linetype, name = "Country")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = seq(0,16,2), minor_breaks = seq(0,16, .5))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  labs(y = NULL)+
  labs(y = "Deaths (7-day average, per 1 million)", x = "2020", title = "Deaths from COVID-19", subtitle = "7-day rolling average per 1 million of population", caption = "Source: European Centre for Disease Prevention and Control\nM - Mandatory national lockdown \nV - Voluntary lockdown or stay home request")
# g5
ggsave(paste0(prints_folder,"death_7da_per1m.jpg"),g5,"jpg", width = 10, height = 6,dpi = "retina")





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
  filter(source == "ecdc") %>%
  # filter(source == "who") %>%
  # ds1_who %>%
  # filter(days_since_100case <= 50) %>%
  filter(metric %in% c("Cases (cumulative)"))

g6 <- d6 %>%
  ggplot(aes_string(x="days_since_100case", y = "value", group = "country_label",color = "country_label",linetype = "country_label"))+
  geom_line(size = .5, color = "grey40")+
  geom_line(size = .5, alpha = .5)+
  geom_line(size = 3, alpha = .3, linetype = "solid")+
  geom_point(shape = 21,size =4, data = d6 %>% filter(!is.na(lockdown_type)) )+
  geom_text(aes(label = letter_code), size = 3, color = "black")+
  # geom_point(aes(fill = country_label),shape = 21,size =2.5, alpha =.4, color = "black",data = d6 %>% filter(!is.na(lockdown_type)) )+
  # geom_point(aes(shape = lockdown_type))+
  # scale_color_viridis_d(option = "plasma", begin = .2, end = .9, name = "Country")+
  scale_linetype_manual(values = country_linetype, name = "Country")+
  scale_color_manual(values = country_colors, name = "Country")+
  scale_fill_manual(values = country_colors, name = "Country")+
  scale_y_log10(
    breaks = c(100,1000, 10000, 100000, 1000000)
    ,labels = scales::comma
    ,limits = c(100, 1000000)
  ) +
  scale_x_continuous(limits = c(0,50))+
# g6
  labs(y = "Cases (cumulative), log", x = "Days since 100th case", title = "Confirmed cases of COVID-19", subtitle = "Cumulative count "
       # , caption = "Source: World Health Organization\nM - Mandatory national lockdown \nV - Voluntary lockdown or stay home request"
       , caption = "Source: European Centre for Disease Control\nM - Mandatory national lockdown \nV - Voluntary lockdown or stay home request"
       )
# g2
ggsave(
  # "./analysis/heap-4-countries/2020-11-25/cases_cum_log-WHO.jpg"
  "./analysis/heap-4-countries/2020-11-25/cases_cum_log-ECDC.jpg"
  ,g6,"jpg", width = 9, height = 6,dpi = "retina")



d6 <-
  ds1 %>% # ECCD
  filter(source == "ecdc") %>%
  # filter(source == "who") %>%
  # ds1_who %>%
  # filter(days_since_100case <= 50) %>%
  filter(metric %in% c("Cases (7DA/1M)"))

g6a <- d6 %>%
  ggplot(aes_string(x="days_since_100case", y = "value", group = "country_label",color = "country_label",linetype = "country_label"))+
  geom_line(size = .5, color = "grey40")+
  geom_line(size = .5, alpha = .5)+
  geom_line(size = 3, alpha = .3, linetype = "solid")+
  geom_point(shape = 21,size =4, data = d6 %>% filter(!is.na(lockdown_type)) )+
  geom_text(aes(label = letter_code), size = 3, color = "black")+
  # geom_point(aes(fill = country_label),shape = 21,size =2.5, alpha =.4, color = "black",data = d6 %>% filter(!is.na(lockdown_type)) )+
  # geom_point(aes(shape = lockdown_type))+
  # scale_color_viridis_d(option = "plasma", begin = .2, end = .9, name = "Country")+
  scale_linetype_manual(values = country_linetype, name = "Country")+
  scale_color_manual(values = country_colors, name = "Country")+
  scale_fill_manual(values = country_colors, name = "Country")+
  # scale_y_log10(
  #   breaks = c(100,1000, 10000, 100000, 1000000)
  #   ,labels = scales::comma
  #   ,limits = c(100, 1000000)
  # ) +
  scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50))+
  scale_x_continuous(limits = c(0,50))+
  # g6
  labs(y = "Confirmed cases", x = "Days since 100th case", title = "Confirmed cases of COVID-19", subtitle = "7-day rolling average per 1 million population"

       , caption = "Source: European Centre for Disease Control\nM - Mandatory national lockdown \nV - Voluntary lockdown or stay home request"
  )
g6a
ggsave(
  # "./analysis/heap-4-countries/2020-11-25/cases_cum_log-WHO.jpg"
  "./analysis/heap-4-countries/2020-11-25/cases_7DAper1M-ECDC.jpg"
  ,g6a,"jpg", width = 9, height = 6,dpi = "retina")


# ----- gemma-scatter -------------
# scatter
ds_lockdown

# d <- ds_epi %>%
#   left_join(ds_lockdown) %>%
#   filter(source == "who") %>%
#   filter(!is.na(letter_code)) %>%
#   t()
#
# ds_who %>%
#   filter(country_code == "IRL") %>%
#   filter(date > as.Date("2020-03-05"), date < as.Date("2020-03-20")) %>%
#   ggplot(aes(x = date, y = n_cases)) +
#   geom_col()+
#   geom_text(aes(label = n_cases))


ds2 %>% glimpse()
# ds_epi %>% glimpse()
ds2 %>% group_by(country_label, source) %>% summarize(
  max_date_death = max(days_since_10death,na.rm = T)
  ,max_date_case = max(days_since_100case,na.rm = T)
)
ds2 %>%
  group_by(source) %>%
  filter(days_since_10death == 0L) %>%
  # filter(days_since_100case == 0L) %>%
  select(country_label, date) %>% distinct()

d7 <- ds2 %>%
  # filter(source == "ecdc") %>%
  filter(source == "who") %>%
  filter(metric %in% c("Deaths (7DA/1M)", "Cases (7DA/1M)")) %>%
  # filter(date == as.Date("2020-11-01")) %>%
  filter(!is.na(letter_code)) %>%
  # filter(days_since_1case %in% seq(20, 240, 20)) %>%
  # filter(days_since_10death %in% seq(0, 260, 10)) %>%
  tidyr::pivot_wider(names_from = "metric", values_from = "value")
d7 %>% glimpse()
countries4 <- c("USA","GBR", "IRL","CAN")
g7 <- d7 %>%
  ggplot(aes(x =`Cases (7DA/1M)`, y =`Deaths (7DA/1M)`,color = lockdown_label))+

  geom_text(aes(label = format(date, "%b-%d")), vjust = -2.3, alpha = .2, color = "black", size = 3)+   geom_point(shape =15, size = 12, data = d7 %>% filter(country_code %in% countries4) )+
  geom_point(shape =0, size = 12)+
  geom_text(aes(label = country_code), data = d7 %>% filter(!country_code %in% countries4))+
  geom_text(aes(label = country_code), data = d7 %>% filter(country_code %in% countries4), color = "white")+
  expand_limits( y= 1.1)+
  scale_y_continuous(breaks = seq(0,1,.2))+
  scale_x_continuous(breaks = seq(0,40,10), limits = c(0,40))+
  scale_color_brewer(type = "qual", palette = "Dark2")+
  labs(
    title = "COVID-19:confirmed cases and deaths at the time of national lockdown or stay home orders", subtitle = "7-day rolling average per 1 million population",y = "Deaths", x = "Cases", caption = "Source: European Centre for Disease Control", color = "Type of lockdown"
  )
  # facet_wrap(~days_since_100case)
  # facet_wrap(~days_since_10death)
g7

ggsave(
  "./analysis/heap-4-countries/2020-11-25/case-v-deaths-at-lockdown.jpg"
  # "./analysis/heap-4-countries/2020-11-25/cases_cum_log-ECDC.jpg"
  ,g7,"jpg", width = 9, height = 6,dpi = "retina")





# ----- timeseries-with-events ----------------
# remake with a different time metric
d8 <- ds1 %>%
  filter(source == "ecdc") %>%
  filter(metric %in% c("Cases (7DA/1M)"))

g8 <- d8 %>%
  ggplot(aes_string(x="days_since_100case", y = "value", group = "country_label",color = "country_label",linetype = "country_label"))+
  geom_line(size = .5, color = "grey40")+
  geom_line(size = .5, alpha = .7)+
  geom_line(size = 3, alpha = .3, linetype = "solid")+
  scale_linetype_manual(values = country_linetype, name = "Country")+
  scale_color_manual(values = country_colors, name = "Country")+
  geom_point(shape = 21,size =4, data = d8 %>% filter(!is.na(lockdown_type)) )+
  geom_text(aes(label = letter_code), size = 3, color = "black")+
  scale_y_continuous(labels = scales::comma_format(), breaks = seq(0,550, 50), minor_breaks = seq(0,550, 10))+
  # scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  scale_x_continuous(limits = c(0,265), breaks = seq(0,300,50), minor_breaks = seq(0,300,10))+
  labs(y = NULL)+
  labs(y = "Cases (7-day average, per 1 million)", x = "Days since 100th case", title = "Confirmed cases of COVID-19", subtitle = "7-day rolling average per 1 million of population", caption = "Source: European Centre for Disease Prevention and Control\nM - Mandatory national lockdown \nV - Voluntary lockdown or stay home request")
# g4
ggsave("./analysis/heap-4-countries/2020-11-25/cases_7da_per1m_since100case.jpg",g8,"jpg", width = 10, height = 6,dpi = "retina")

g8a <- d8 %>%
  filter(days_since_100case <= 50) %>%
  ggplot(aes_string(x="days_since_100case", y = "value", group = "country_label",color = "country_label",linetype = "country_label"))+
  geom_line(size = .5, color = "grey40")+
  geom_line(size = .5, alpha = .7)+
  geom_line(size = 3, alpha = .3, linetype = "solid")+
  scale_linetype_manual(values = country_linetype, name = "Country")+
  scale_color_manual(values = country_colors, name = "Country")+
  geom_point(shape = 21,size =4, data = d8 %>% filter(!is.na(lockdown_type)) )+
  geom_text(aes(label = letter_code), size = 3, color = "black")+
  scale_y_continuous(labels = scales::comma_format(), breaks = seq(0,250, 10), minor_breaks = seq(0,250, 10))+
  # scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  scale_x_continuous(limits = c(0,50), breaks = seq(0,300,5), minor_breaks = seq(0,300,1))+
  labs(y = NULL)+
  labs(y = "Cases (7-day average, per 1 million)", x = "Days since 100th case", title = "Confirmed cases of COVID-19", subtitle = "7-day rolling average per 1 million of population", caption = "Source: European Centre for Disease Prevention and Control\nM - Mandatory national lockdown \nV - Voluntary lockdown or stay home request")
# g4
ggsave("./analysis/heap-4-countries/2020-11-25/cases_7da_per1m_since100case_first50.jpg",g8a,"jpg", width = 10, height = 6,dpi = "retina")

d9 <- ds1 %>%
  filter(source == "ecdc") %>%
  filter(metric %in% c("Deaths (7DA/1M)"))

g9 <- d9 %>%
  ggplot(aes_string(x="days_since_10death", y = "value", group = "country_label",color = "country_label",linetype = "country_label"))+
  geom_line(size = .5, color = "grey40")+
  geom_line(size = .5, alpha = .5)+
  geom_line(size = 3, alpha = .3, linetype = "solid")+
  geom_point(shape = 21,size =4, data = d9 %>% filter(!is.na(lockdown_type)) )+
  geom_text(aes(label = letter_code), size = 3, color = "black")+
  scale_color_manual(values = country_colors, name = "Country")+
  scale_linetype_manual(values = country_linetype, name = "Country")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = seq(0,16,2), minor_breaks = seq(0,16, .5))+
  # scale_x_date(date_breaks = "1 month", date_labels = "%b" )+
  scale_x_continuous(limits = c(-10,265), breaks = seq(0,300,50), minor_breaks = seq(0,300,10))+
  labs(y = NULL)+
  labs(y = "Deaths (7-day average, per 1 million)", x = "Days since 10th death", title = "Deaths from COVID-19", subtitle = "7-day rolling average per 1 million of population", caption = "Source: European Centre for Disease Prevention and Control\nM - Mandatory national lockdown \nV - Voluntary lockdown or stay home request")
# g5
ggsave("./analysis/heap-4-countries/2020-11-25/death_7da_per1m_since10death.jpg",g9,"jpg", width = 10, height = 6,dpi = "retina")


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




