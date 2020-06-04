rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(utils)
library(httr)
library(dplyr)
# loadNamespace("dplyr")
library(OECD) # see vignette https://cran.r-project.org/web/packages/OECD/vignettes/oecd_vignette_main.html
library(rsdmx)
# ---- declare-globals ---------------------------------------------------------
config <- config::get()
# ---- load-data ---------------------------------------------------------------
ds_country <-
  readr::read_csv(
    config$path_country
  ) %>%
  dplyr::filter(desired)

# # ECDC
# # path_save <- paste0("./data-unshared/derived/ocdc-",Sys.Date(),".csv")
ds_covid <- readr::read_csv(config$path_input_covid)
# ds_covid %>% glimpse()


ls <- readr::read_rds(paste0(config$path_oecd_clean,"family.rds"))
list_object <- ls
ds_meta <-  list_object$data %>% dplyr::distinct(IND,UNIT) %>% tibble::as_tibble()
ds_meta <- ds_meta %>%
  dplyr::left_join(list_object$structure[["IND"]], by = c("IND" = "id")) %>%
  dplyr::rename(var_label = label) %>%
  dplyr::left_join(list_object$structure$UNIT, by = c("UNIT" = "id")) %>%
  dplyr::rename(unit_label = label) %>%
  # dplyr::rename( VAR = IND ) %>%
  dplyr::arrange(IND,UNIT)
# d_var_unit %>% neat_DT()
ds_meta %>% glimpse()
# divorce - FAM4B
# marriage - FAM4A
ds <- ls$data %>% tibble::as_tibble() %>%
  # filter(IND %in% c("FAM4A","FAM4B")) %>%
  filter(COU %in% (ds_country %>% pull(id)) ) %>%
  dplyr::group_by(COU, IND) %>%
  dplyr::summarize(
    min_year = min(obsTime,na.rm=T)
    ,max_year = max(obsTime,na.rm=T)
    # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,mean = mean(obsValue, na.rm = T)
    ,median = median(obsValue, na.rm = T)
    ,value = sum(mean, median)/2
  ) %>%
  dplyr::left_join(ds_meta, by = "IND")


ds1 <- ds_covid %>% compute_epi_timeline(n_deaths_first_day = 0) %>%
  filter(epi_timeline == 30) %>%
  select(country_code, country, n_deaths, n_cases, n_population_2018) %>%
  dplyr::left_join(
    ds %>% select(COU, IND, value, UNIT, var_label, unit_label)
    , by = c("country_code" = "COU")
  ) %>%
  mutate(
    n_deaths_per_1m = n_deaths/n_population_2018*1000000
    ,n_cases_per_1m = n_cases/n_population_2018*1000000

  )
ds1 %>% glimpse()

g1 <- ds1 %>%
  ggplot(aes(x=value, y = n_deaths_per_1m, label = country_code))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~IND, scales = "free")+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    , vjust = 3
  )
g1

a <- c("FAM2","FAM4A","FAM4B","FAM10B","FAM10A", "FAM20")

g1 <- ds1 %>%
  filter(IND %in% a) %>%
  # ggplot(aes(x=value, y = n_deaths, label = country_code))+
  ggplot(aes(x=value, y = n_deaths_per_1m, label = country_code))+
  geom_point(shape = 21, fill = NA, alpha = 1, color = "salmon", size = 2)+
  geom_text()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~var_label, scales = "free")+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    , vjust = 3
  )
g1

inds <- ds1 %>% pull(IND) %>% unique()
length(inds)
g1 <- ds1 %>%
  # filter(IND %in% inds[1:C20x]) %>%
  filter(IND %in% inds[21:41]) %>%
  ggplot(aes(x=value, y = n_deaths_per_1m, label = country_code))+
  # ggplot(aes(x=value, y = n_deaths, label = country_code))+
  # ggplot(aes(x=value, y = n_cases_per_1m, label = country_code))+
  # ggplot(aes(x=value, y = n_cases, label = country_code))+
  geom_point(shape = 21, fill = NA, alpha = 1, color = "salmon", size = 2)+
  geom_text()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~var_label, scales = "free")+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, color = "salmon", alpha = .6
    , vjust = 1
  )
g1




# compute_rank <- function(
  list_object <- ls
  var_label_i <- "FAM4A"
  unit_label_i

  d_country <-  ds_country
  # list_object <- ls_input_health$health_resources
  # var_name <- "HOPITBED"
  # unit_name     <- "RTOINPNB"
  # var_label_i <- "Practising caring personnel"
  # unit_label_i <- "Density per 1 000 population (head counts)"

  # d_country <-
  #   readr::read_csv(
  #     # config$path_country
  #     "data-public/metadata/oecd/country.csv"
  #   ) %>%
  #   dplyr::filter(desired)

  var_unit <- list_object %>%
    get_var_unit_lookup() %>%
    # dplyr::filter(VAR == var_name, UNIT == unit_name)
    dplyr::filter(var_label == var_label_i, unit_label == unit_label_i)

  var_name <- var_unit %>% dplyr::pull(VAR)
  unit_name <- var_unit %>% dplyr::pull(UNIT)

  d_measure <- list_object$data %>%
    dplyr::filter(VAR == var_name, UNIT == unit_name ) %>%
    # dplyr::filter(var_label == var_label_i, unit_label == unit_label_i ) %>%
    dplyr::filter(COU %in% (d_country %>% dplyr::filter(desired) %>%  dplyr::pull(id)) ) %>%
    dplyr::group_by(COU) %>%
    dplyr::summarize(
      min_year = min(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
      ,max_year = max(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
      ,mean = mean(obsValue, na.rm = T)
      ,median = median(obsValue, na.rm = T)
      ,value = sum(mean, median)/2

    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      rank_percentile = dplyr::percent_rank(value)
      ,rank = dplyr::dense_rank(value)
      ,n_tile = dplyr::ntile(value, 5)
    )
  var_unit <- dplyr::bind_rows(
    var_unit,
    tibble::as_tibble(  as.data.frame(matrix(nrow = nrow(d_measure)-1, ncol=ncol(var_unit))) )
  ) %>%
    dplyr::select(names(var_unit)) %>%
    tidyr::fill(names(var_unit))
  d_out <- var_unit %>% dplyr::bind_cols(d_measure)
  return(d_out)



# ---- define-functions ----------------------------------
# function to get a list of unique variables and units of measurement along with descriptive labels
# get_var_unit_lookup <- function(list_object){
#   # list_object <- ls_input_health$health_resources
#
#   d_var_unit <-  list_object$data %>% dplyr::distinct(VAR,UNIT) %>% tibble::as_tibble()
#   d_var_unit <- d_var_unit %>%
#     dplyr::left_join(list_object$structure$VAR, by = c("VAR" = "id")) %>%
#     dplyr::rename(var_label = label) %>%
#     dplyr::left_join(list_object$structure$UNIT, by = c("UNIT" = "id")) %>%
#     dplyr::rename(unit_label = label) %>%
#     dplyr::arrange(VAR,UNIT)
#   return(d_var_unit)
# }
# How to use
# dvars_health_resources <- ls_input_health$health_resources %>% get_var_unit_lookup()
# dvars_health_status <- ls_input_health$health_status %>% get_var_unit_lookup()

# dvars_health_resources %>% neat_DT()

# ---- compute-rank-function --------------
# function to compute ranks of countries for a given measure

# compute_rank <- function(list_object, var_name, unit_name, d_country = ds_country){
#   # list_object <- ls_input_health$health_resources
#   # var_name <- "HOPITBED"
#   # unit_name     <- "RTOINPNB"
#
#   var_unit <- list_object %>%
#     get_var_unit_lookup() %>%
#     dplyr::filter(VAR == var_name, UNIT == unit_name)
#
#   d_measure <- list_object$data %>%
#     dplyr::filter(VAR == var_name, UNIT == unit_name ) %>%
#     dplyr::filter(COU %in% (d_country %>% dplyr::filter(desired) %>%  dplyr::pull(id)) ) %>%
#     dplyr::group_by(COU) %>%
#     dplyr::summarize(
#       min_year = min(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
#       ,max_year = max(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
#       ,mean = mean(obsValue, na.rm = T)
#       ,median = median(obsValue, na.rm = T)
#       ,value = sum(mean, median)/2
#
#     ) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(
#       rank_percentile = dplyr::percent_rank(value)
#       ,rank = dplyr::dense_rank(value)
#       ,n_tile = dplyr::ntile(value, 5)
#     )
#   var_unit <- dplyr::bind_rows(
#     var_unit,
#     tibble::as_tibble(  as.data.frame(matrix(nrow = nrow(d_measure)-1, ncol=ncol(var_unit))) )
#   ) %>%
#     dplyr::select(names(var_unit)) %>%
#     tidyr::fill(names(var_unit))
#   d_out <- var_unit %>% dplyr::bind_cols(d_measure)
#   return(d_out)
# }
# How to use
# d_measure <- ls_input_health$health_resources %>% compute_rank("HOPITBED","RTOINPNB")



