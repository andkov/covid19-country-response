# This script reads two files: patient event table + location map.
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(knitr) # dynamic documents
library(rmarkdown) # dynamic
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
# library(TabularManifest) # exploratory data analysis, see https://github.com/Melinae/TabularManifest
requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables
# requireNamespace("plyr", quietly=TRUE)
# requireNamespace("reshape2", quietly=TRUE) #For converting wide to long
# requireNamespace("mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.

# ---- load-sources ------------------------------------------------------------
# browser()

setwd("../..") # This is a hack.  Figure out the right way.

config <- config::get(config="../../config.yml")
source("./scripts/common-functions.R")        # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions
source("./scripts/graphing/graph-support.R") # font and color conventions

# ---- load-data -------------------------------------------------------------


# COVID
# path_save <- paste0("./data-unshared/derived/ocdc-",Sys.Date(),".csv")
ds_covid <- readr::read_csv(config$path_input_covid)
ds_covid %>% glimpse()

# OEDC
# path_folder_oecd_health <- "./data-unshared/raw/oecd/health/"
input_files_oecd_health <- list.files(config$path_oecd_health_chapter, pattern = ".rds$",  full.names = T)
file_names <- gsub(".rds$","", basename(input_files_oecd_health))

ls_input_health <- list()
for(i in seq_along(input_files_oecd_health)){
    file_name_i <- gsub(".rds$","", basename(input_files_oecd_health[i]))
    ls_input_health[[file_name_i]] <- readr::read_rds(input_files_oecd_health[i])
}
# ---- inspect-data -------------------------------------------------------------

# ---- tweak-data --------------------------------------------------------------
# to help look for relevant variables and measures in this chaptert
# d_meta <- ls_input_health$health_resources %>% get_var_unit_lookup()
# d_meta <- ls_input_health$health_status %>% get_var_unit_lookup()

ls_input_health$health_resources %>%
    get_var_unit_lookup() %>%
    # pull(VAR) %>%
    pull(UNIT) %>%
    unique() %>%
    dput()

#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # ds_country <-
    #     readr::read_csv(
    #         config$path_country
    #     ) %>%
    #     dplyr::filter(desired)

    output$var_name <- renderText({
        paste("You chose", input$var_name)
    })

    slidersLayout <- reactive({
        return(list(
            var_name = (input$var_name),
            unit_name = (input$unit_name)
            # DriveTimeOuter = as.integer(input$driveTimeOuter),
            # RadiusFactorCity = (100 * 2^(-1+input$radiusFactorCity)), #Control how big the city circles are.
            # RadiusFactorCompetitor = (200 * 2^(-1+input$radiusFactorCompetitor)), #Control how big the competitor circles are.
            # ExistingStoreOpacity = input$existingStoreOpacity,
            # ProposedStoreOpacity = input$proposedStoreOpacity,
            # LongitudinalLines = input$longitudinalLines,
            # InflatedGraphs = (input$inflatedGraphs=="1")
        ))
    })

    # var_name    <- c("HOPITBED", "HOPITBED2")[1]
    # unit_name   <- "RTOINPNB"

    prep_data_trajectory <- function(ls_oecd, df_covid, n_deaths_first_day = 1, var_name, unit_name){
        # browser()
        var_name <- slidersLayout()$var_name
        unit_name <- slidersLayout()$unit_name

        d_covid <- compute_epi_timeline(df_covid, n_deaths_first_day = n_deaths_first_day)#, d_country = ds_country)
        d_oecd  <- ls_oecd %>% compute_rank(var_name = var_name, unit_name =unit_name)
        d_out <-   dplyr::left_join(
            d_covid, d_oecd, by = c("country_code" = "COU")
        )
        return(d_out)
    }

    output$spaghetti_1 <- renderPlotly({
    # how to use
        d_measure <- prep_data_trajectory(
            ls_oecd = ls_input_health$health_resources
            ,df_covid = ds_covid
            ,n_deaths_first_day = 1
            ,var_name = var_name
            ,unit_name = unit_name
        )

        g <- d_measure %>%
            # dplyr::filter(epi_timeline <=30) %>%
            # ggplot(aes(x = epi_timeline, y = n_deaths, color = rank_percentile)) +
            ggplot(aes(x = epi_timeline, y = n_deaths, color = factor(n_tile))) +
            geom_line(aes(group = country_code))+
            theme_minimal()+
            facet_wrap(~n_tile)+
            labs(
                title = paste0(slidersLayout()$var_name," - ", slidersLayout()$unit_name)
            )
        spaghetti_1 <- plotly::ggplotly(g)
        # spaghetti_1 <-
    })

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
