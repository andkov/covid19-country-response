#' ---
#' author: Andriy Koval
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#'
#' ---

#+ include = FALSE
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console


# ---- knitr-opts --------------------------------------------------------------
#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
knitr::opts_knit$set(root.dir = "../")

# ---- load-sources ------------------------------------------------------------

#' # Load Packages
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

#' Declare Globals
# ---- declare-globals ---------------------------------------------------------

year_key <- c(
  NULL    = "1"
  ,NULL   = "2"
  ,"2010" =  "3"
  ,"2011" =  "4"
  ,"2012" =  "5"
  ,"2013" =  "6"
  ,"2014" =  "7"
  ,"2015" =  "8"
  ,"2016" =  "9"
  ,"2017" = "10"
  ,"2018" = "11"
  ,"2019" = "12"
)

age_group_key <- c(
  "Total"    = "0"
  ,"0-4"     = "1"
  ,"5-9"     = "2"
  ,"10-14"   = "3"
  ,"15-19"   = "4"
  ,"20-24"   = "5"
  ,"25-29"   = "6"
  ,"30-34"   = "7"
  ,"35-39"   = "8"
  ,"40-44"   = "9"
  ,"45-49"   = "10"
  ,"50-54"   = "11"
  ,"55-59"   = "12"
  ,"60-64"   = "13"
  ,"65-69"   = "14"
  ,"70-74"   = "15"
  ,"75-79"   = "16"
  ,"80-84"   = "17"
  ,"85-over" = "18"
)


col_key <- c(
  "state_fips"                         = "STATE"
  ,"county_fips"                       = "COUNTY"
  ,"year"                              = "YEAR"
  ,"age_group"                         = "AGEGRP"
  ,"total_population"                  = "TOT_POP"
  ,"total_male_population"             = "TOT_MALE"
  ,"total_female_population"           = "TOT_FEMALE"
  ,"white_male_population"             = "WA_MALE"
  ,"white_female_population"           = "WA_FEMALE"
  ,"black_male_population"             = "BA_MALE"
  ,"black_female_population"           = "BA_FEMALE"
  ,"american_indian_male_population"   = "IA_MALE"
  ,"american_indian_female_population" = "IA_FEMALE"
  ,"asian_male_population"             = "AA_MALE"
  ,"asian_female_population"           = "AA_FEMALE"
  ,"native_hawaiian_male_population"   = "NA_MALE"
  ,"native_hawaiian_female_population" = "NA_FEMALE"
  ,"not_hispanic_male_population"      = "NH_MALE"
  ,"not_hispanic_female_population"    = "NH_FEMALE"
  ,"hispanic_male_population"          = "H_MALE"
  ,"hispanic_female_population"        = "H_FEMALE"
)

age_filter <- c(
  "20-24"
  ,"25-29"
  ,"30-34"
  ,"35-39"
  ,"40-44"
  ,"45-49"
  ,"50-54"
  ,"55-59"
  ,"60-64"
  ,"65-69"
  ,"70-74"
  ,"75-79"
  ,"80-84"
  ,"85-over"
)




age_groups <- list(
  age_20_44  = c("20-24" ,"25-29" ,"30-34" ,"35-39" ,"40-44" )
  ,age_45_64 = c("45-49","50-54","55-59","60-64")
  ,age_65_74 = c("65-69","70-74")
  ,age_75_over = c( "75-79" ,"80-84" ,"85-over")
)


#' # Load Data
# ---- load-data ---------------------------------------------------------------

ds_estimates_raw <- read_csv(
  "./data-unshared/raw/us-census/us-population-estimate-2010-2019-age-race.csv"
  )

#' # Tweak Data
# ---- tweak-data -------------------------------------------------------------

ds_estimates <- ds_estimates_raw %>%
  select(all_of(col_key)) %>%
  unite(col = "county_fips", state_fips:county_fips, sep = "") %>%
  mutate(across(c(year,age_group),as_factor)
         ,across(year, ~fct_recode(.,!!!year_key))
         ,across(age_group, ~fct_recode(.,!!!age_group_key))) %>%
  drop_na(year)


ds1 <- ds_estimates %>% filter(age_group %in% age_filter) %>%
  group_by(year, county_fips) %>%
  summarise(across(where(is.numeric), sum),.groups = "keep" ) %>%
  summarise(
    total_population = total_population
    ,across(
      where(is.numeric) & !total_population
      ,~(.x / total_population)*100
      ,.names = "adult_pct_{col}"
    )
    ,.groups = "keep"
  ) %>% ungroup()

ds1 %>% glimpse()


# AGe group percents

ds_total <- ds_estimates %>%
  select(county_fips, year, age_group,total_population) %>%
  filter(age_group %in% age_filter) %>%
  group_by(year, county_fips) %>%
  summarise(across(total_population, sum))

ls_age_groups <- list()

for(item_i in seq_along(age_groups)){
  column_name <- names(age_groups[item_i])

  ls_age_groups[[item_i]] <-  ds_estimates %>%
    select(county_fips, year, age_group,total_population) %>%
    filter(age_group %in% age_groups[[item_i]]) %>%
    group_by(year, county_fips) %>%
    summarise(total = sum(total_population)
    ) %>%
    rename_with(.fn = ~paste0(.,"_",column_name), .cols = total)

}


ds_age_group <- ds_total %>%
  left_join(ls_age_groups[[1]]) %>%
  left_join(ls_age_groups[[2]]) %>%
  left_join(ls_age_groups[[3]]) %>%
  left_join(ls_age_groups[[4]]) %>%
  group_by(year, county_fips) %>%
  summarise(
    total_population = total_population
    ,across(
      where(is.numeric) & !total_population
      ,~(.x / total_population)*100
      ,.names = "pct_{col}"
    )
    ,.groups = "keep"
  ) %>% ungroup()


#' # Join
# ---- join-data ---------------------------------------------------------------


ds_combined <- ds1 %>%
  left_join(ds_age_group, by = c("year", "county_fips", "total_population"))




#' # Save Data
# ---- save-data ---------------------------------------------------------------

ds_combined %>% write_rds("./data-public/derived/us-pop-estimate-2010-2019.rds"
                  ,compress = "gz")

# ds1 %>% write_csv(
#   gzfile("./data-public/derived/us-pop-estimate-2010-2019.csv"))


