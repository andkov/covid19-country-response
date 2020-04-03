#these libraries need to be loaded
library(utils)
library(httr)
library(magrittr)
library(dplyr)

# Source
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into “R”. The dataset will be called "data".
ds <- read.csv(tf)
ds %>% glimpse()


names(ds) <- c("date", "day", "month", "year", "n_cases", "n_deaths", "country", "geo_id", "country_code","n_population_2018")

ds <- ds %>%
  dplyr::mutate(
    date = lubridate::dmy(date)
    # dd =
  )
ds %>% glimpse()

ds %>% dplyr::distinct( country)

ds %>% dplyr::distinct(date) %>% arrange(date)
