# remotes::install_github("expersso/OECD")
library(OECD)

ds_list <- tibble::as_tibble(OECD::get_datasets())

dataset <- "DUR_D"
dstruc <- OECD::get_data_structure(dataset)
str(dstruc, max.level = 2)
# dput(dstruc$AGE$id) #c("1519", "1524", "2024", "2554", "5564", "6599", "900000")
# dput(dstruc$COUNTRY$id)
# c("AUS", "AUT", "BEL", "CAN", "CZE", "DNK", "EST", "FIN", "FRA",
# "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR",
# "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT",
# "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "OECD",
# "CRI", "EU22", "EU16", "EU28", "EUR", "G7", "NAM", "OCE", "BGR",
# "HRV", "CYP", "MKD", "MLT", "ROU", "RUS", "ZAF", "COL")

ages <- c("1519", "2024", "2554", "5564", "6599")
countries <- c(
  "AUS", "AUT", "BEL", "CAN", "CZE", "DNK", "EST", "FIN", "FRA",
"DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR",
"LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT",
"SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "OECD",
"CRI", "EU22", "EU16", "EU28", "EUR", "G7", "NAM", "OCE", "BGR",
"HRV", "CYP", "MKD", "MLT", "ROU", "RUS", "ZAF", "COL")
# cat(paste(dstruc$COUNTRY$id, collapse = "\n"))
# readr::write_csv(dstruc$COUNTRY, "data-public/metadata/oecd/country.csv")

filter_list <- list(
  c("DEU", "FRA"),
  # "MW",
  c("MEN", "WOMEN"),
  # "900000"
  ages
)
df <-
  OECD::get_dataset(
    dataset = dataset,
    filter = filter_list,
    start_time = 2015, end_time = 2017
  )
