# https://www.r-bloggers.com/tracking-covid19-cases-throughout-nj-with-r/

library(readr) #loads package

covid19_data<-read_csv("us_counties.csv") #function that reads in csv files

library(dplyr) # used for data wrangling

NJ_covid19<-covid19_data%>%
  dplyr::filter(state == "New Jersey",county != "Unknown") #filters data frame

head(NJ_covid19) # Returns the first or last parts of the data frame



names(NJ_counties)<-tolower(names(NJ_counties)) # Makes county column header lowercase

NJ_counties$county<-tolower(NJ_counties$county)#Makes all rows in the county column lowercase

NJ_covid19$county<-tolower(NJ_covid19$county)#Makes all rows in the county column lowercase
Now I can join the two datasets based on the county column in both.

NJ_covid19_shapes<-left_join(NJ_covid19,NJ_counties,by="county")%>%
  dplyr::select(date,county,state,cases,deaths,geometry)#selects only the columns of interest

head(NJ_covid19_shapes)
