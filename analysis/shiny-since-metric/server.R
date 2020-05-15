# This script reads two files: patient event table + location map.
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(knitr) # dynamic documents
library(rmarkdown) # dynamic
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
library(shiny)
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

# setwd("../..") # This is a hack.  Figure out the right way.

# config <- config::get(config="../../config.yml")
config <- config::get(config="config.yml")
# source("./scripts/common-functions.R")        # reporting functions and quick views
# source("./scripts/graphing/graph-presets.R") # font and color conventions
# source("./scripts/graphing/graph-support.R") # font and color conventions

ggplot2::theme_set(ggplot2::theme_bw())
# ---- load-data -------------------------------------------------------------

# ls <- readr::read_rds("../../analysis/shiny-since-metric/data.rds")
ls <- readr::read_rds("data.rds")
ds_wide <- ls[["wide"]]
ds_long <- ls[["long"]]

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
    available_metrics <-  ds_long$metric %>% unique()
    selectInput("var_name_x", "Choose Variable X", choices = available_metrics, selected = available_metrics[1])
    selectInput("var_name_y", "Choose Variable X", choices = available_metrics, selected = available_metrics[13])

    output$metricPlot <- renderPlotly({
        var_name_x <- input$var_name_x
        var_name_y <- input$var_name_y

        g <- ds_wide %>%
            ggplot(aes_string(
                x = var_name_x
                , y = var_name_y
                , label = "contry_code2"
                , group = "country_label"))+
            geom_text()
        g <- plotly::ggplotly(g)
        g %>% print()
    })

})
