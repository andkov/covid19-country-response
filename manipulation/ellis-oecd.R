rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R")
# dvars_health_resources <- ls_input_health$health_resources %>% get_var_unit_lookup()
# d_covid <- ds_covid %>% compute_epi_timeline(n_deaths_first_day = 1)
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

path_folder_oecd_health <- "./data-unshared/raw/oecd/health/"


# ---- load-data ---------------------------------------------------------------
ds_covid <- readr::read_csv(config$path_input_covid)


input_files_oecd_health <- list.files(path_folder_oecd_health, pattern = ".rds$",  full.names = T)
file_names <- gsub(".rds$","", basename(input_files_oecd_health))

if (length(input_files_oecd_health) == 0L)
  stop("No rds files were found in `", path_folder_oecd_health, "`.")

ls_input_health <- list()
for(i in seq_along(input_files_oecd_health)){
  file_name_i <- gsub(".rds$","", basename(input_files_oecd_health[i]))
  ls_input_health[[file_name_i]] <- readr::read_rds(input_files_oecd_health[i])
}

# what countries should be in focus?
ds_country <-
  readr::read_csv(
    config$path_country
  ) %>%
  dplyr::filter(desired)

# ----- family ----------------------

ls <- readr::read_rds(paste0(config$path_oecd_out,"family.rds"))
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
ds0 <- ls$data %>% tibble::as_tibble() %>%
  # filter(IND %in% c("FAM4A","FAM4B")) %>%
  filter(COU %in% (ds_country %>% pull(id)) ) %>%
  dplyr::group_by(COU, IND, SEX) %>%
  dplyr::summarize(
    min_year = min(obsTime,na.rm=T)
    ,max_year = max(obsTime,na.rm=T)
    # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,mean = mean(obsValue, na.rm = T)
    ,median = median(obsValue, na.rm = T)
    ,value = sum(mean, median)/2
  ) %>%
  dplyr::left_join(ds_meta, by = "IND")

ds0 %>% distinct(IND, UNIT, SEX) %>% arrange(IND) %>% print(n=Inf)


# ---- employment  --------------------------
ls <- readr::read_rds(paste0(config$path_oecd_out,"employment.rds"))
list_object <- ls
ds0 <- ls$data %>% tibble::as_tibble()
lsmeta <- ls$structure

ds0 %>% glimpse()
lsmeta  %>% str(1)
lsmeta$SUBJECT
lsmeta$UNIT
lsmeta$OBS_STATUS
#
ds0 %>% group_by(SUBJECT,FREQUENCY, TIME_FORMAT, POWERCODE, OBS_STATUS) %>% count()
ds0 %>% group_by(SUBJECT,OBS_STATUS) %>% count()
lsmeta$OBS_STATUS

ds1 <- ds0 %>%
  dplyr::group_by(LOCATION, SUBJECT) %>%
  dplyr::summarize(
    min_year = min(obsTime,na.rm=T)
    ,max_year = max(obsTime,na.rm=T)
    # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,mean = mean(obsValue, na.rm = T)
    ,median = median(obsValue, na.rm = T)
    ,value = sum(mean, median)/2
  ) %>%
  dplyr::left_join(lsmeta$SUBJECT, by = c("SUBJECT"="id" ) )
ds1 %>% glimpse()
ds2 <- ds_covid %>%
  compute_epi_timeline(n_deaths_first_day = 0) %>%
  filter(epi_timeline == 30) %>%
  dplyr::right_join()



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



