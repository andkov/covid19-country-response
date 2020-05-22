config <- config::get()

# focal from OECD database
ds_country <- readr::read_csv(config$path_country)
# temp, with edited labels
ds_country_codes <- readr::read_csv(config$path_country_codes)

# https://datahub.io/JohnSnowLabs/country-and-continent-codes-list
ds_geo <- read.csv("https://datahub.io/JohnSnowLabs/country-and-continent-codes-list/r/country-and-continent-codes-list-csv.csv", stringsAsFactors = F)
names(ds_geo) <- tolower(names(ds_geo))
ds_geo %>% glimpse()

ds_geo <- ds_geo %>% dplyr::left_join(
  ds_country_codes %>% select(country_label, country_code3)
  , by = c("three_letter_country_code" = "country_code3")
) %>%
  mutate(
    continent_code = ifelse(continent_name == "North America","NAM", continent_code)
  ) %>%
  tibble::as_tibble() %>%
  rename(
    "country_code" = "three_letter_country_code"
    ,"country_code2" = "two_letter_country_code"
  ) %>%
  dplyr::mutate(
    oecd = country_code %in% (ds_country %>% filter(desired) %>% pull(id) )
  ) %>%
  # custom label tweaks
  dplyr::mutate(
    country_label = stringr::str_replace_all(country_label, "Korea \\(the Republic of\\)", "South Korea")
  ) %>%
  # make decisions about continents
  dplyr::filter( !(country_code == "AZE" & continent_name == "Europe" ) )
ds_geo %>% glimpse()

two_continents <-
  ds_geo %>% group_by(country_code) %>% count() %>%
  filter(n>1) %>% pull(country_code) %>% na.omit() %>% as.character()

ds_geo %>% readr::write_csv("./data-public/metadata/world-geography.csv")

ds_geo %>% filter(country_code == "KOR") %>% t()
