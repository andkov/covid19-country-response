rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
config <- config::get()

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(utils)
library(httr)
library(dplyr)
loadNamespace("dplyr")
library(OECD) # see vignette https://cran.r-project.org/web/packages/OECD/vignettes/oecd_vignette_main.html
library(rsdmx)
# ---- declare-globals ---------------------------------------------------------
path_folder_oecd_health <- "./data-unshared/raw/oecd/health/"


# ---- load-data ---------------------------------------------------------------
input_files_oecd_health <- list.files(path_folder_oecd_health, pattern = ".rds$",  full.names = T)
file_names <- gsub(".rds$","", basename(input_files_oecd_health))

ls_input_health <- list()
for(i in seq_along(input_files_oecd_health)){
  file_name_i <- gsub(".rds$","", basename(input_files_oecd_health[i]))
  ls_input_health[[file_name_i]] <- readr::read_rds(input_files_oecd_health[i])
}

dstruc <- ls_input_health$health_resources$structure
# ---- define-functions ----------------------------------
# function to get a list of unique variables and units of measurement along with descriptive labels
get_var_unit_lookup <- function(list_object){
  # list_object <- ls_input_health$health_resources

  d_var_unit <-  list_object$data %>% dplyr::distinct(VAR,UNIT) %>% tibble::as_tibble()
  d_var_unit <- d_var_unit %>%
    dplyr::left_join(list_object$structure$VAR, by = c("VAR" = "id")) %>%
    dplyr::rename(var_label = label) %>%
    dplyr::left_join(list_object$structure$UNIT, by = c("UNIT" = "id")) %>%
    dplyr::rename(unit_label = label) %>%
    dplyr::arrange(VAR,UNIT)
  return(d_var_unit)
}
# How to use
dvars_health_resources <- ls_input_health$health_resources %>% get_var_unit_lookup()
dvars_health_status <- ls_input_health$health_status %>% get_var_unit_lookup()




