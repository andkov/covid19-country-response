library(OECD)

d <- OECD::get_datasets()
d <- OECD::get_dataset()

ds_health_status <- OECD::get_dataset("HEALTH_STAT")


ds_meta <- OECD::browse_metadata("HEALTH_STAT")

OECD::get_data_structure("HEALTH_STAT")


dataset_list <- get_datasets()

search_dataset("health", data = dataset_list)

health_stat <- get_data_structure("HEALTH_STAT")

ds_health_status <- get_dataset("")
