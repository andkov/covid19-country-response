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

# path_folder_oecd_health <- "./data-unshared/raw/oecd/health/"


# ---- load-data ---------------------------------------------------------------
ds_covid <- readr::read_csv(config$path_input_covid) %>%
  compute_epi_timeline()
ds_covid %>% glimpse()
#
# input_files_oecd_health <- list.files(path_folder_oecd_health, pattern = ".rds$",  full.names = T)
# file_names <- gsub(".rds$","", basename(input_files_oecd_health))
#
# if (length(input_files_oecd_health) == 0L)
#   stop("No rds files were found in `", path_folder_oecd_health, "`.")
#
# ls_input_health <- list()
# for(i in seq_along(input_files_oecd_health)){
#   file_name_i <- gsub(".rds$","", basename(input_files_oecd_health[i]))
#   ls_input_health[[file_name_i]] <- readr::read_rds(input_files_oecd_health[i])
# }

# what countries should be in focus?
ds_country <-
  readr::read_csv(
    config$path_country
  ) %>%
  dplyr::filter(desired)

# ----- family ----------------------
file_keyword <- "family"
(path_read  <- paste0(config$path_oecd_raw,file_keyword,".rds"))
(path_write <- paste0(config$path_oecd_clean,file_keyword,".rds"))
ls <- readr::read_rds(path_read)
ds0 <- ls$data %>% tibble::as_tibble()
lsmeta <- ls$structure

ds0 %>% glimpse()
ds0 %>% head()
lsmeta  %>% str(1)
lsmeta$IND
lsmeta$UNIT
lsmeta$SEX
lsmeta$TIME_FORMAT
lsmeta$POWERCODE
ds1 <- ds0 %>%
  dplyr::mutate(
    indicator = factor(IND, levels = lsmeta$IND$id, labels = lsmeta$IND$label)
    ,unit     = factor(UNIT, levels = lsmeta$UNIT$id, labels = lsmeta$UNIT$label)
    ,SEX = factor(SEX, levels = lsmeta$SEX$id, labels = lsmeta$SEX$label)
    ,TIME_FORMAT = factor(TIME_FORMAT, levels = lsmeta$TIME_FORMAT$id, labels = lsmeta$TIME_FORMAT$label)
    ,POWERCODE = factor(POWERCODE, levels = lsmeta$POWERCODE$id, labels = lsmeta$POWERCODE$label)
  )
ds1 %>% glimpse()

# what defines a row?
ds1 %>% group_by(COU,SEX,indicator,UNIT) %>% count()
ds1 %>% group_by(TIME_FORMAT) %>% count() # single = Annual
ds1 %>% group_by(POWERCODE) %>% count() # single = Units
grouping_set <- c("location","indicator","unit","SEX")
ds2 <- ds1 %>%
  dplyr::rename(
    location = COU
  ) %>%
  dplyr::group_by(.dots = grouping_set) %>%
  dplyr::summarize(
    min_year = min(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,max_year = max(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,mean = mean(obsValue, na.rm = T)
    ,median = median(obsValue, na.rm = T)
    ,value = sum(mean, median)/2
  ) %>%
  dplyr::ungroup()
ds2 %>% glimpse()
ls[["data_clean"]] <- ds1
ls[["data_agg"]] <- ds2
ls %>%  readr::write_rds(path_write)


# ---- employment  --------------------------
file_keyword <- "employment"
(path_read  <- paste0(config$path_oecd_raw,file_keyword,".rds"))
(path_write <- paste0(config$path_oecd_clean,file_keyword,".rds"))
ls <- readr::read_rds(path_read)
ds0 <- ls$data %>% tibble::as_tibble()
lsmeta <- ls$structure

ds0 %>% glimpse()
ds0 %>% head()
lsmeta  %>% str(1)
lsmeta$SUBJECT
lsmeta$FREQUENCY
lsmeta$TIME_FORMAT; ds0$TIME_FORMAT %>% unique()
lsmeta$POWERCODE;ds0$POWERCODE %>% unique()

#
ds1 <- ds0 %>%
  dplyr::mutate(
    indicator = factor(SUBJECT, levels = lsmeta$SUBJECT$id, labels = lsmeta$SUBJECT$label)
    ,FREQUENCY = factor(FREQUENCY, levels = lsmeta$FREQUENCY$id, labels = lsmeta$FREQUENCY$label)
    ,TIME_FORMAT = factor(TIME_FORMAT, levels = lsmeta$TIME_FORMAT$id, labels = lsmeta$TIME_FORMAT$label)
    ,POWERCODE = factor(POWERCODE, levels = lsmeta$POWERCODE$id, labels = lsmeta$POWERCODE$label)
    ,OBS_STATUS = factor(OBS_STATUS, levels = lsmeta$OBS_STATUS$id, labels = lsmeta$OBS_STATUS$label)
  )
ds1 %>% glimpse()

# what defines a row?
ds1 %>% group_by(LOCATION,indicator,obsTime) %>% count() %>% arrange(desc(n))
grouping_set <- c("location","indicator")
ds2 <- ds1 %>%
  dplyr::rename(location = LOCATION) %>%
  dplyr::group_by(.dots = grouping_set) %>%
  dplyr::summarize(
    min_year = min(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,max_year = max(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,mean = mean(obsValue, na.rm = T)
    ,median = median(obsValue, na.rm = T)
    ,value = sum(mean, median)/2
  ) %>%
  dplyr::ungroup()
ds2 %>% glimpse()
ls[["data_clean"]] <- ds1
ls[["data_agg"]] <- ds2
ls %>%  readr::write_rds(path_write)

# ----- immigration -----------------
# # Immigrants by citizenship and age
# file_keyword <- "immigration"
# (path_read  <- paste0(config$path_oecd_raw,file_keyword,".rds"))
# (path_write <- paste0(config$path_oecd_clean,file_keyword,".rds"))
# ls <- readr::read_rds(path_read)
#
# ds0 <- ls$data %>% tibble::as_tibble()
# lsmeta <- ls$structure
#
# ds0 %>% glimpse()
# ds0 %>% head()
# lsmeta  %>% str(1)
# lsmeta$VAR_DESC
# lsmeta$COUB
# lsmeta$FBORN
# lsmeta$EDU
# lsmeta$AGE
# lsmeta$NAT
# lsmeta$OBS_STATUS
# #
# ds1 <- ds0 %>%
#   dplyr::mutate(
#     FBORN  = factor(FBORN, levels = lsmeta$FBORN$id, labels = lsmeta$FBORN$label)
#     ,EDU   = factor(EDU, levels = lsmeta$EDU$id, labels = lsmeta$EDU$label)
#     ,AGE   = factor(AGE, levels = lsmeta$AGE$id, labels = lsmeta$AGE$label)
#     ,NAT   = factor(NAT, levels = lsmeta$NAT$id, labels = lsmeta$NAT$label)
#   )
# # maybe too old ( data from 2000, only 15yo and older)
# ds1 %>%
#   filter(
#     COU   == "AUT",
#     FBORN == "All places of birth",
#     EDU   == "All levels of education",
#     NAT   == "All citizenships",
#     AGE   == "All ages"
#   )
# ds1 %>% readr::write_rds(path_write)
# ---- education --------------------------
# Education attainment of 25 - 65 year olds
file_keyword <- "educational_attainment"
(path_read  <- paste0(config$path_oecd_raw,file_keyword,".rds"))
(path_write <- paste0(config$path_oecd_clean,file_keyword,".rds"))
ls <- readr::read_rds(path_read)

ds0 <- ls$data %>% tibble::as_tibble()
lsmeta <- ls$structure

ds0 %>% glimpse()
ds0 %>% head()
lsmeta  %>% str(1)
lsmeta$VAR_DESC
lsmeta$ISC11A
lsmeta$SEX
lsmeta$AGE
lsmeta$FIELD
lsmeta$MEASURE
lsmeta$INDICATOR
lsmeta$OBS_STATUS
#
ds1 <- ds0 %>%
  dplyr::mutate(
    ISC11A     = factor(ISC11A, levels = lsmeta$ISC11A$id, labels = lsmeta$ISC11A$label)
    ,SEX       = factor(SEX, levels = lsmeta$SEX$id, labels = lsmeta$SEX$label)
    ,AGE       = factor(AGE, levels = lsmeta$AGE$id, labels = lsmeta$AGE$label)
    ,FIELD     = factor(FIELD, levels = lsmeta$FIELD$id, labels = lsmeta$FIELD$label)
    ,MEASURE   = factor(MEASURE, levels = lsmeta$MEASURE$id, labels = lsmeta$MEASURE$label)
    ,OBS_STATUS = factor(OBS_STATUS, levels = lsmeta$OBS_STATUS$id, labels = lsmeta$OBS_STATUS$label)
    ,indicator = factor(INDICATOR, levels = lsmeta$INDICATOR$id, labels = lsmeta$INDICATOR$label)
    ,unit = factor(UNIT, levels = lsmeta$UNIT$id, labels = lsmeta$UNIT$label)

  )
ds1 %>% glimpse()
# what defines a row?
ds1 %>% group_by(COUNTRY, indicator, SEX, AGE,ISC11A, MEASURE, obsTime ) %>% count() %>% arrange(desc(n))
grouping_set <- c("location", "indicator","unit", "MEASURE", "ISC11A", "SEX","AGE")
ds2 <- ds1 %>%
  dplyr::rename(location = COUNTRY) %>%
  dplyr::filter(MEASURE == "Value") %>%
  dplyr::group_by(.dots = grouping_set) %>%
  dplyr::summarize(
    min_year = min(REFERENCEPERIOD,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,max_year = max(REFERENCEPERIOD,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,mean = mean(obsValue, na.rm = T)
    ,median = median(obsValue, na.rm = T)
    ,value = sum(mean, median)/2
  ) %>%
  dplyr::ungroup()
ds2 %>% glimpse()
ls[["data_clean"]] <- ds1
ls[["data_agg"]] <- ds2
ls %>%  readr::write_rds(path_write)


# ---- population -------
file_keyword <- "population"
(path_read  <- paste0(config$path_oecd_raw,file_keyword,".rds"))
(path_write <- paste0(config$path_oecd_clean,file_keyword,".rds"))
ls <- readr::read_rds(path_read)

ds0 <- ls$data %>% tibble::as_tibble()
lsmeta <- ls$structure

ds0 %>% glimpse()
ds0 %>% head()
lsmeta  %>% str(1)
lsmeta$LOCATION
lsmeta$SEX
lsmeta$AGE
#
ds1 <- ds0 %>%
  dplyr::mutate(
     SEX       = factor(SEX, levels = lsmeta$SEX$id, labels = lsmeta$SEX$label)
    ,AGE       = factor(AGE, levels = lsmeta$AGE$id, labels = lsmeta$AGE$label)
    ,TIME_FORMAT = factor(TIME_FORMAT, levels = lsmeta$TIME_FORMAT$id, labels = lsmeta$TIME_FORMAT$label)
  )

ds1 %>% glimpse()

# what defines a row?
ds1 %>% group_by(LOCATION,SEX,AGE,obsTime) %>% count() %>% arrange(desc(n))
grouping_set <- c("location", "SEX","AGE")
ds2 <- ds1 %>%
  dplyr::rename(location = LOCATION) %>%
  dplyr::group_by(.dots = grouping_set) %>%
  dplyr::summarize(
    min_year = min(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,max_year = max(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,mean = mean(obsValue, na.rm = T)
    ,median = median(obsValue, na.rm = T)
    ,value = sum(mean, median)/2
  ) %>%
  dplyr::ungroup()
ds2 %>% glimpse()
ls[["data_clean"]] <- ds1
ls[["data_agg"]] <- ds2
ls %>%  readr::write_rds(path_write)

# ---- serving_citizens -------
file_keyword <- "serving_citizens"
(path_read  <- paste0(config$path_oecd_raw,file_keyword,".rds"))
(path_write <- paste0(config$path_oecd_clean,file_keyword,".rds"))
ls <- readr::read_rds(path_read)

ds0 <- ls$data %>% tibble::as_tibble()
lsmeta <- ls$structure

ds0 %>% glimpse()
ds0 %>% head()
lsmeta  %>% str(1)
lsmeta$IND
lsmeta$TIME_FORMAT

#
ds1 <- ds0 %>%
  dplyr::mutate(
    indicator    = factor(IND, levels = lsmeta$IND$id, labels = lsmeta$IND$label)
    ,TIME_FORMAT = factor(TIME_FORMAT, levels = lsmeta$TIME_FORMAT$id, labels = lsmeta$TIME_FORMAT$label)
  )

ds1 %>% glimpse()
# what defines a row?
ds1 %>% group_by(COU,indicator, obsTime) %>% count() %>% arrange(desc(n))
grouping_set <- c("location", "indicator")
ds2 <- ds1 %>%
  dplyr::rename(location = COU) %>%
  dplyr::group_by(.dots = grouping_set) %>%
  dplyr::summarize(
    min_year = min(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,max_year = max(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,mean = mean(obsValue, na.rm = T)
    ,median = median(obsValue, na.rm = T)
    ,value = sum(mean, median)/2
  ) %>%
  dplyr::ungroup()
ds2 %>% glimpse()
ls[["data_clean"]] <- ds1
ls[["data_agg"]] <- ds2
ls %>%  readr::write_rds(path_write)

# ---- better_life_index -------
file_keyword <- "better_life_index"
(path_read  <- paste0(config$path_oecd_raw,file_keyword,".rds"))
(path_write <- paste0(config$path_oecd_clean,file_keyword,".rds"))
ls <- readr::read_rds(path_read)

ds0 <- ls$data %>% tibble::as_tibble()
lsmeta <- ls$structure

ds0 %>% glimpse()
ds0 %>% head()
lsmeta  %>% str(1)
lsmeta$INDICATOR
lsmeta$MEASURE
lsmeta$INEQUALITY
lsmeta$UNIT
lsmeta$POWERCODE

#
ds1 <- ds0 %>%
  dplyr::mutate(
    indicator   = factor(INDICATOR, levels = lsmeta$INDICATOR$id, labels = lsmeta$INDICATOR$label)
    ,MEASURE    = factor(MEASURE, levels = lsmeta$MEASURE$id, labels = lsmeta$MEASURE$label)
    ,INEQUALITY = factor(INEQUALITY, levels = lsmeta$INEQUALITY$id, labels = lsmeta$INEQUALITY$label)
    ,UNIT       = factor(UNIT, levels = lsmeta$UNIT$id, labels = lsmeta$UNIT$label)
    ,POWERCODE  = factor(POWERCODE, levels = lsmeta$POWERCODE$id, labels = lsmeta$POWERCODE$label)
  )
ds1 %>% glimpse()
# what defines a row?
ds1 %>% group_by(LOCATION,indicator, INEQUALITY) %>% count() %>% arrange(desc(n))
grouping_set <- c("location", "indicator","INEQUALITY")
ds2 <- ds1 %>%
  dplyr::rename(location = LOCATION) %>%
  dplyr::group_by(.dots = grouping_set) %>%
  dplyr::summarize(
    # min_year = min(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    # ,max_year = max(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
     mean = mean(obsValue, na.rm = T)
    ,median = median(obsValue, na.rm = T)
    ,value = sum(mean, median)/2
  ) %>%
  dplyr::ungroup()
ds2 %>% glimpse()
ls[["data_clean"]] <- ds1
ls[["data_agg"]] <- ds2
ls %>%  readr::write_rds(path_write)

# ---- health_resources -------
file_keyword <- "health_resources"
(path_read  <- paste0(config$path_oecd_raw,file_keyword,".rds"))
(path_write <- paste0(config$path_oecd_clean,file_keyword,".rds"))
ls <- readr::read_rds(path_read)

ds0 <- ls$data %>% tibble::as_tibble()
lsmeta <- ls$structure

ds0 %>% glimpse()
ds0 %>% head()
lsmeta  %>% str(1)
lsmeta$VAR
lsmeta$UNIT
lsmeta$TIME_FORMAT
lsmeta$OBS_STATUS

#
ds1 <- ds0 %>%
  dplyr::mutate(
    indicator    = factor(VAR, levels = lsmeta$VAR$id, labels = lsmeta$VAR$label)
    ,unit    = factor(UNIT, levels = lsmeta$UNIT$id, labels = lsmeta$UNIT$label)
    ,TIME_FORMAT = factor(TIME_FORMAT, levels = lsmeta$TIME_FORMAT$id, labels = lsmeta$TIME_FORMAT$label)
    ,OBS_STATUS = factor(OBS_STATUS, levels = lsmeta$OBS_STATUS$id, labels = lsmeta$OBS_STATUS$label)
  )
ds1 %>% glimpse()

# what defines a row?
ds1 %>% group_by(COU, indicator, unit, obsTime) %>% count() %>% arrange(desc(n))
grouping_set <- c("location", "indicator","unit")
ds2 <- ds1 %>%
  dplyr::rename(location = COU) %>%
  dplyr::group_by(.dots = grouping_set) %>%
  dplyr::summarize(
    min_year = min(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,max_year = max(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,mean = mean(obsValue, na.rm = T)
    ,median = median(obsValue, na.rm = T)
    ,value = sum(mean, median)/2
  ) %>%
  dplyr::ungroup()
ds2 %>% glimpse()
ls[["data_clean"]] <- ds1
ls[["data_agg"]] <- ds2
ls %>%  readr::write_rds(path_write)
# ---- health_status -------
file_keyword <- "health_status"
(path_read  <- paste0(config$path_oecd_raw,file_keyword,".rds"))
(path_write <- paste0(config$path_oecd_clean,file_keyword,".rds"))
ls <- readr::read_rds(path_read)

ds0 <- ls$data %>% tibble::as_tibble()
lsmeta <- ls$structure

ds0 %>% glimpse()
ds0 %>% head()
lsmeta  %>% str(1)
lsmeta$VAR
lsmeta$UNIT
lsmeta$TIME_FORMAT
lsmeta$OBS_STATUS

#
ds1 <- ds0 %>%
  dplyr::mutate(
    indicator    = factor(VAR, levels = lsmeta$VAR$id, labels = lsmeta$VAR$label)
    ,unit    = factor(UNIT, levels = lsmeta$UNIT$id, labels = lsmeta$UNIT$label)
    ,TIME_FORMAT = factor(TIME_FORMAT, levels = lsmeta$TIME_FORMAT$id, labels = lsmeta$TIME_FORMAT$label)
    ,OBS_STATUS = factor(OBS_STATUS, levels = lsmeta$OBS_STATUS$id, labels = lsmeta$OBS_STATUS$label)
  )
ds1 %>% glimpse()
# what defines a row?
ds1 %>% group_by(COU, indicator, unit, obsTime) %>% count() %>% arrange(desc(n))
d <- ds1 %>%
  filter(
    COU == "AUS",
    indicator == "Good/very good health, females aged 15+",
    unit == "% of population (crude rate)",
    obsTime == "2017"
  ) %>% distinct()
dt <- d %>% t() %>% as_tibble()
dt %>% mutate(same = (V1 == V2))
# looks like the same label is assigned to different VAR (difference in methodology?)
ds1 %>% group_by(COU,VAR, indicator, unit, obsTime) %>% count() %>% arrange(desc(n))

grouping_set <- c("location","VAR", "indicator","unit")
ds2 <- ds1 %>%
  dplyr::rename(location = COU) %>%
  dplyr::group_by(.dots = grouping_set) %>%
  dplyr::summarize(
    min_year = min(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,max_year = max(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
    ,mean = mean(obsValue, na.rm = T)
    ,median = median(obsValue, na.rm = T)
    ,value = sum(mean, median)/2
  ) %>%
  dplyr::ungroup()
ds2 %>% glimpse()
ls[["data_clean"]] <- ds1
ls[["data_agg"]] <- ds2
ls %>%  readr::write_rds(path_write)

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



