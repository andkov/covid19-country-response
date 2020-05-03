rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

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
config <- config::get()

# ---- load-data ---------------------------------------------------------------
dataset_list <- OECD::get_datasets()
search_dataset("health", data = dataset_list)
search_dataset("ethn", data = dataset_list)
search_dataset("Educational attainment", data = dataset_list) %>% print(n = nrow(.))

# ---- define-query-function -------------------

get_oecd_data <- function(
  url_dsmx
  ,dataset_name
  ,list_label
){
  dstruc <- OECD::get_data_structure(dataset_name)
  data <- rsdmx::readSDMX(url_dsmx) %>% as.data.frame()
  ls_object <- list(
    "name" = dataset_name
    ,"structure" = dstruc
    ,"url" = url_dsmx
    ,"data" = data
  )
  str(dstruc, max.level = 1) %>% print()
  pryr::object_size(ls_object) %>% print()
  readr::write_rds(ls_object, fs::path(config$path_oecd_out, paste0(list_label,".rds")))
}

# ALFS Summary tables
"https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/ALFS_SUMTAB/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA19+EU28+G-7+OECD+BRA+COL+CRI+RUS.YG+YGTT07L1_ST+YGTT07L1_IXOB+YGFE07L1_ST+YGMA07L1_ST+YGTT08L1_ST+YGTT08L1_IXOB+YGFE08L1_ST+YGFE08PE_ST+YGMA08L1_ST+YGMA08PE_ST+YGTT09L1_ST+YGTT09L1_IXOB+YGTT09PE_ST+YGTT10L1_ST+YGTT10L1_IXOB+YGTT10PE_ST+YGTT11L1_ST+YGTT11L1_IXOB+YGTT11PE_ST+YGTT22L1_ST+YGTT22P1_ST+YGFE22P1_ST+YGMA22P1_ST+YGTT06L1_ST+YGFE06L1_ST+YGMA06L1_ST+YGTT06PC_ST+YGFE06P2_ST+YGMA06P2_ST.A/all?startTime=2014&endTime=2018" %>%
  get_oecd_data("ALFS_SUMTAB", "employment")


# Immigrants by citizenship and age
# Demography and Population --> Migration Statistics --> DIOC --> Immigrants by citizenship and age
# This database contains information on several demographic and labour market characteristics of the population of 28 OECD countries around the year 2000, by country of birth. (maybe too old)
"https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/DIOC_CITIZEN_AGE/AFRI+ASIA+EURO+NOAM+OCEA+SCAC+OTHER+ALL_COUB.1+0+99+ALL_FBORN.1+2+3_4+99+ALL_EDU.1_2+3_10+11_12+99+ALL_AGE.1+2+99+ALL_NAT.AUS+AUT+BEL+CAN+CZE+DNK+FIN+FRA+GRC+HUN+IRL+ITA+JPN+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+ESP+SWE+CHE+TUR+GBR+USA+OTO/all?" %>%
  get_oecd_data("DIOC_CITIZEN_AGE", "immigration")

# Education attainment of 25 - 65 year olds
"https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EAG_NEAC/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+OAVG+G20+NMEC+ARG+BRA+CHN+COL+CRI+IND+IDN+RUS+SAU+ZAF.L0T2+L0+L1_T_SC3T4+L2_T_SC2+L2_T_SC3T4+L3_T_SC2+L3T4+L3+L4+L5T8+L5+L6+L7+L8.T+F+M.Y25T64.T.VALUE+SE.NEAC_SHARE_EA/all?startTime=9999&endTime=9999" %>%
  get_oecd_data("EAG_NEAC", "educational_attainment")


### Contextual Predisposing Characteristics ####

# Family
"https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/FAMILY/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+BRA+BGR+CHN+COL+CRI+HRV+CYP+IND+IDN+MLT+ROU+RUS+ZAF.TOTAL+MALE+FEMALE.D1+FAM1+FAM2+FAM3+FAM4+FAM4A+FAM4B+FAM5+FAM5A+FAM5B+FAM5C+D2+FAM6+FAM6A+FAM6B+FAM7+FAM8+FAM8A+FAM8B+FAM8C+FAM9+FAM9A+FAM9B+FAM9C+FAM10+FAM10A+FAM10B+FAM10C+D3+FAM11+FAM11A+FAM11B+FAM11C+FAM11D+FAM12+FAM12A+FAM12B+FAM13+FAM14+FAM15+FAM15A+FAM15B+D4+FAM16+FAM16A+FAM16B+FAM17+FAM18+FAM18A+FAM18B+FAM18C+FAM19+FAM19A+FAM19B+FAM19C+FAM19D+FAM20/all?startTime=2014&endTime=2018" %>%
  get_oecd_data("FAMILY","family")


# Population
"https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/HISTPOP/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EU28+G20+OECD+WLD+NMEC+ARG+BRA+BGR+CHN+COL+CRI+HRV+CYP+IND+IDN+MLT+ROU+RUS+SAU+SGP+ZAF.W+M+T.TOTAL+0_4+05_9+10_14+15_19+20_24+25_29+30_34+35_39+40_44+45_49+50_54+55_59+60_64+65_69+70_74+75_79+80_84+85_OVER+50_OVER+LESS_20+15-64+20-64+65_OVER+65_OVER_SHARE+LESS_15_SHARE+15-24_SHARE+OAD15-64+TOTD20-64+15-64_SHARE+POP_GR/all?startTime=2008&endTime=2018" %>%
  get_oecd_data("HISTPOP","population")

# Population projection
"https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/POPPROJ/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EU28+G20+OECD+WLD+NMEC+ARG+BRA+BGR+CHN+COL+CRI+HRV+CYP+IND+IDN+MLT+ROU+RUS+SAU+SGP+ZAF.W+M+T.TOTAL+0_4+05_9+10_14+15_19+20_24+25_29+30_34+35_39+40_44+45_49+50_54+55_59+60_64+65_69+70_74+75_79+80_84+85_OVER+50_OVER+LESS_20+15-64+20-64+65_OVER+65_OVER_SHARE+LESS_15_SHARE+15-24_SHARE+OAD15-64+TOTD20-64/all?startTime=2018&endTime=2020" %>%
  get_oecd_data("POPPROJ","population_projections")

# ---- define-queries ---------------------

if( !fs::dir_exists(config$path_oecd_health_chapter) )
  fs::dir_create(config$path_oecd_health_chapter)

# Population
dataset <- "HISTPOP"
dstruc <- OECD::get_data_structure(dataset)
str(dstruc, max.level = 1)
query_url_SDMX <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/HISTPOP/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EU28+G20+OECD+WLD+NMEC+ARG+BRA+BGR+CHN+COL+CRI+HRV+CYP+IND+IDN+MLT+ROU+RUS+SAU+SGP+ZAF.W+M+T.TOTAL+0_4+05_9+10_14+15_19+20_24+25_29+30_34+35_39+40_44+45_49+50_54+55_59+60_64+65_69+70_74+75_79+80_84+85_OVER+50_OVER+LESS_20+15-64+20-64+65_OVER+65_OVER_SHARE+LESS_15_SHARE+15-24_SHARE+OAD15-64+TOTD20-64+15-64_SHARE+POP_GR/all?startTime=2008&endTime=2018"
data <- rsdmx::readSDMX(query_url_SDMX) %>% as.data.frame()
ls_population <- list(
  "name" = dataset
  ,"structure" = dstruc
  ,"url" = query_url_SDMX
  ,"data" = data
)
pryr::object_size(ls_population)
readr::write_rds(ls_population, fs::path(config$path_oecd_health_chapter, "population.rds"))



# Population
dataset <- "HISTPOP"
dstruc <- OECD::get_data_structure(dataset)
str(dstruc, max.level = 1)
query_url_SDMX <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/HISTPOP/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EU28+G20+OECD+WLD+NMEC+ARG+BRA+BGR+CHN+COL+CRI+HRV+CYP+IND+IDN+MLT+ROU+RUS+SAU+SGP+ZAF.W+M+T.TOTAL+0_4+05_9+10_14+15_19+20_24+25_29+30_34+35_39+40_44+45_49+50_54+55_59+60_64+65_69+70_74+75_79+80_84+85_OVER+50_OVER+LESS_20+15-64+20-64+65_OVER+65_OVER_SHARE+LESS_15_SHARE+15-24_SHARE+OAD15-64+TOTD20-64+15-64_SHARE+POP_GR/all?startTime=2008&endTime=2018"
data <- rsdmx::readSDMX(query_url_SDMX) %>% as.data.frame()
ls_population <- list(
  "name" = dataset
  ,"structure" = dstruc
  ,"url" = query_url_SDMX
  ,"data" = data
)
pryr::object_size(ls_population)
readr::write_rds(ls_population, fs::path(config$path_oecd_health_chapter, "population.rds"))



# Health Care Resources
dataset <- "HEALTH_REAC"
dstruc <- OECD::get_data_structure(dataset)
str(dstruc, max.level = 1)
query_url_SDMX <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/HEALTH_REAC/GEHE+GEHETHSM+HEPL+PHYS+PHYSMEDE+PHYSPAPS+PHYSREGP+PAGG+PAGGFEMM+PAGGFU35+PAGGF344+PAGGF454+PAGGF564+PAGGF65O+PAGGF75O+PAGGHOMM+PAGGMU35+PAGGM344+PAGGM454+PAGGM564+PAGGM65O+PAGGM75O+PAGGTOPY+PAGGTU35+PAGGT344+PAGGT454+PAGGT564+PAGGT65O+PAGGT75O+EMPL+EMPLGENE+EMPLGENP+EMPLOTGP+EMPLSPMP+EMPLPEDI+EMPLGYNE+EMPLPSYS+EMPLSPEC+EMPLSURG+EMPLOTSP+EMPLOTPH+MIDW+MIDWMIDW+MIDWMIPA+MIDWMILP+MINU+MINUINFI+MINUQUAL+MINUASSO+MINUPANU+MINUPAPN+MINUPAAP+MINUNULP+MINULPPN+MINULPAP+CARE+CARECPRA+CARECPRO+DNST+DNSTDENT+DNSTPADN+DNSTLPDN+PHST+PHSTPHAR+PHSTPAPH+PHSTLPPH+PSIO+PSIOPHTH+HOEM+HOEMHEMP+HOEMHPHY+HOEMHNUR+HOEMHASN+HOEMHHCA+HOEMHOTH+HOEMHOTS+HEDU+HEDUMEGR+HEDUDNGR+HEDUPHGR+HEDUMWGR+HEDUNUGR+HEDUPNGR+HEDUAPGR+RVNU+RVNURMEG+RVNURMED+RVNURINF+HEPT+HOSP+HOSPTHOS+HOSPPUHO+HOSPNPHO+HOSPFRHO+HOSPGHOS+HOPI+HOPITBED+HOPILICS+HOPIREHA+HOPILIMM+HOPIOBED+HOPILIPS+HOPITPOH+HOPINROH+HOPIFROH+IPIN+IPINSCAN+IPINSCAH+IPINSCAA+IPINMRIM+IPINMRIH+IPINMRIA+IPINPETS+IPINPETH+IPINPETA+IPINGAMA+IPINGAMH+IPINGACA+IPINMAMO+IPINMAMH+IPINMAMA+IPINRTEQ+IPINRTEH+IPINRTEA.PERSMYNB+DENSPPNB+PEREMPNB+PHYTOTNB+PHYU35NB+PHY344NB+PHY454NB+PHY564NB+PHYT65NB+PHYT75NB+MILBIRNB+PERMEDNB+FTEEMPEF+DENSEFEF+PTHEMPNB+PFHEMPEF+NOMBRENB+PERPHYNB+YSALARMT+YSALXRMT+YSALPPMT+YSALGDMT+YSELFPMB+YSEFXRMB+YSEFPPMB+YSEFGDMB+NBMILPNB+HOPRAT+HOPFTE+NURRAT+NURFTE+LTCOVRNB+RTOINPNB+RTOALLNB+YSALAWMT+YSEFAWMT+YSALPIMT+YSEFPIMT.AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+BRA+CHN+COL+CRI+IND+IDN+RUS+ZAF/all?startTime=2015&endTime=2018"
data <- rsdmx::readSDMX(query_url_SDMX) %>% as.data.frame()

ls_health_resources <- list(
  "name" = dataset
  ,"structure" = dstruc
  ,"url" = query_url_SDMX
  ,"data" = data
)
pryr::object_size(ls_health_resources)
readr::write_rds(ls_health_resources, fs::path(config$path_oecd_health_chapter, "health_resources.rds"))


# Health Status
dataset <- "HEALTH_STAT"
dstruc <- OECD::get_data_structure(dataset)
# str(dstruc, max.level = 1)
query_url_SDMX <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/HEALTH_STAT/EVIE+EVIEFE00+EVIEFE40+EVIEFE60+EVIEFE65+EVIEFE80+EVIEHO00+EVIEHO40+EVIEHO60+EVIEHO65+EVIEHO80+EVIETOTA+CICD+CICDALLC+CICDINFE+CICDTBLS+CICDHIVD+CICDNEOP+CICDTUME+CICDCANC+CICDNCRA+CICDMNBR+CICDSTOM+CICDPANC+CICDMNPR+CICDLIVR+CICDCRVX+CICDOVRY+CICDHODG+CICDLEUK+CICDBLAD+CICDSKIN+CICDSANG+CICDENDO+CICDDBTM+CICDTROU+CICDDMTA+CICDALCO+CICDSUBS+CICDNERV+CICDPARK+CICDALZH+CICDCIRC+CICDISCH+CICDMYOC+CICDCERV+CICDREPS+CICDINPN+CICDPNEU+CICDBAEM+CICDASMA+CICDDIGE+CICDPEPT+CICDCIRR+CICDPEAU+CICDOSTE+CICDGENI+CICDGROS+CICDAFFE+CICDCONG+CICDSYMP+CICDEXTC+CICDACCD+CICDTRAC+CICDCHUT+CICDPOSN+CICDHARM+CICDHOCD+MATI+MATIINFA+MATIINTW+MATINEON+MATINETW+MATIPERI+MATIMATM+PLYL+PLYLALLC+PLYLINFE+PLYLTBLS+PLYLHIVD+PLYLNEOP+PLYLTUME+PLYLCANC+PLYLNCRA+PLYLMNBR+PLYLSTOM+PLYLPANC+PLYLMNPR+PLYLLIVR+PLYLCRVX+PLYLOVRY+PLYLHODG+PLYLLEUK+PLYLBLAD+PLYLSKIN+PLYLSANG+PLYLENDO+PLYLDBTM+PLYLTROU+PLYLDMTA+PLYLALCO+PLYLSUBS+PLYLNERV+PLYLPARK+PLYLALZH+PLYLCIRC+PLYLISCH+PLYLMYOC+PLYLCERV+PLYLREPS+PLYLINPN+PLYLPNEU+PLYLBAEM+PLYLASMA+PLYLDIGE+PLYLPEPT+PLYLCIRR+PLYLPEAU+PLYLOSTE+PLYLGENI+PLYLGROS+PLYLAFFE+PLYLCONG+PLYLSYMP+PLYLEXTC+PLYLACCD+PLYLTRAC+PLYLCHUT+PLYLPOSN+PLYLHARM+PLYLHOCD+AVDM+AVDMPRVM+AVDMTRTM+PRHS+PRHSFGHE+PRHSFFAH+PRHSFBAH+PRHSMGHE+PRHSMFAH+PRHSMBAH+PRHSTGHE+PRHSTFAH+PRHSTBAH+SRHS+SRHSFGHA+SRHSFGHB+SRHSFGHC+SRHSFGHD+SRHSFGHE+SRHSMGHA+SRHSMGHB+SRHSMGHC+SRHSMGHD+SRHSMGHE+SRHSTGHA+SRHSTGHB+SRHSTGHC+SRHSTGHD+SRHSTGHE+SREC+SRECGHQO+SRECGHQV+SRECFE02+SRECFE34+SRECFE56+SRECMA02+SRECMA34+SRECMA56+SRECTO02+SRECTO34+SRECTO56+INFA+INFAPREM+COMD+COMDAIDS+COMDIPER+COMDIMEA+COMDIHPB+CANC+CANCTOCA+CANCCOLC+CANCLUNC+CANCBREC+CANCCEIX+CANCPROC+INJR+INJRACIR+DISA+DISASRAB+DISACPAB.EVIDUREV+EVIFHOEV+EVIHFEEV+NBFEMEPF+NBMALEPH+NBPOPUPC+TXCRUDTF+TXCRUDTH+TXCRUDTX+TXCMFETF+TXCMHOTH+TXCMILTX+RATEPTTX+TXMILBTH+TXCMMMTX+ANNFEMTF+ANNHOMTH+ANNTOTTX+PERCALEF+LIVBRTTX+NEWCASTX+PERCMTTX+NBWOMAPF+NBMANYPH+NBPOPIPC+INCITFTF+INCITHTH+BLESSETX+JOUPANNB+NOMBRENB.AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+BRA+CHN+COL+CRI+IND+IDN+RUS+ZAF/all?startTime=2015&endTime=2019"
data <- rsdmx::readSDMX(query_url_SDMX) %>% as.data.frame()

ls_health_status <- list(
  "name" = dataset
  ,"structure" = dstruc
  ,"url" = query_url_SDMX
  ,"data" = data
)
pryr::object_size(ls_health_status)
readr::write_rds(ls_health_status, fs::path(config$path_oecd_health_chapter, "health_status.rds"))

# joining to the metadata in dstruc object
# ds1 <- data %>%
  # dplyr::left_join(
  #   dstruc$VAR
  #   , by = c("VAR"="id")
  # ) %>%
  #
  # dplyr::left_join(
  #   dstruc$UNIT
  #   , by = c("UNIT" = "id")
  # )
#




