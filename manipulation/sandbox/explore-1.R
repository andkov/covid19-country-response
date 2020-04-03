# remotes::install_github("expersso/OECD")
library(OECD)
library(magrittr)
config <- config::get()
ds_list <- tibble::as_tibble(OECD::get_datasets())

dataset <- "DUR_D"
dstruc <- OECD::get_data_structure(dataset)
str(dstruc, max.level = 2)
# dput(dstruc$AGE$id) #c("1519", "1524", "2024", "2554", "5564", "6599", "900000")

# readr::write_csv(dstruc$COUNTRY, "data-public/metadata/oecd/country.csv")


#### ellis starts here
ds_country <-
  readr::read_csv(
    config$path_country
  ) %>%
  dplyr::filter(desired)

filter_list <- list(
  ds_country$id,
  config$oced_genders,
  config$oced_ages
)
df <-
  OECD::get_dataset(
    dataset = dataset,
    filter = filter_list,
    start_time = 2015, end_time = 2017
  )
