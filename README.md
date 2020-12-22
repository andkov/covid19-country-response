# covid19-country-response
Combines data  from OECD database and other source to examine countries' responses to COVID-19 and factors shaping it.

COVID-19 Data Sources
-------------
See [./manipulation/](manipulation/) for the scripts that wrangle the data sets and create analysis-ready forms  
- [European CDC](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide)  
- [John Hopkins](https://github.com/CSSEGISandData/COVID-19)  
- [World Health Organization](https://covid19.who.int/table)
- [Oxford COVID-19 Government Response Tracker](https://github.com/OxCGRT/covid-policy-tracker)

Analytic Reports
----------------------
- [EDA-1][eda-1] - general trends of the COVID-19 spread and Stringency Index 
- [EDA-2][eda-2] - exploring early sequence and toll of COVID-19 among OECD countries
- [EDA-2][eda-3] - sequence of epidemiological events in all countries

[eda-1]:https://raw.githack.com/andkov/covid19-country-response/master/analysis/eda-1/eda-1.html
[eda-2]:https://raw.githack.com/andkov/covid19-country-response/master/analysis/eda-2/eda-2.html
[eda-3]:https://raw.githack.com/andkov/covid19-country-response/master/analysis/eda-3/eda-3.html

- [HEAP Figures](master/analysis/heap-4-countries/heap-4-countries-2.R)

|Cases|Deaths|  
|---|---|  
|![][HEAP1]|![][HEAP2]|  

[HEAP1]:analysis/heap-4-countries/2020-11-30/cases_7da_per1m.jpg
[HEAP2]:analysis/heap-4-countries/2020-11-30/death_7da_per1m.jpg

- [HEPL Figures](analysis/hepl-3-states/hepl-3-states.R) ( [PDF](analysis/hepl-3-states/prints/2020-11-27/Alexander-Unruh-Koval-2020-HEPL-figures.pdf)) 

|Country|Regions| States|   
|---|---|---|  
|![][HEPL1]|![][HEPL2]| ![][HEPL3]| 






[HEPL1]:analysis/hepl-3-states/prints/2020-11-27/01-us-cases-deaths.jpg
[HEPL2]:analysis/hepl-3-states/prints/2020-11-27/02-covid-by-regions.jpg
[HEPL3]:analysis/hepl-3-states/prints/2020-11-27/03-NY-FL-WI.jpg


Dashboards 
------------------
- [COVID explorer](https://kyleb.shinyapps.io/covid-explorer/) links COVID metrics with the results of the US presidential elections in 2016 and 2020.  
- [Since Metrics](https://andkov.shinyapps.io/shiny-since-metric/) exploration tool for relating two given COVID metrics among OECD countries


Potential data sources
----------------------------------

* **Johns Hopkins CSSE**:  https://github.com/CSSEGISandData/COVID-19

  * The `fips` variable would make it easy to link to census shapefiles

  * Dashboard: https://systems.jhu.edu/ 
  * 
* **The Humanitarian Data Exchange**: https://data.humdata.org/search?q=covid
* **Worldometer**: https://www.worldometers.info/coronavirus/
