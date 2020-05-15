# # This script reads two files: patient event table + location map.
# # rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
#
#
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(knitr) # dynamic documents
library(rmarkdown) # dynamic
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
library(shiny)
library(plotly)
# library(TabularManifest) # exploratory data analysis, see https://github.com/Melinae/TabularManifest
requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables
# requireNamespace("plyr", quietly=TRUE)
# requireNamespace("reshape2", quietly=TRUE) #For converting wide to long
# requireNamespace("mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.


# setwd("../..") # This is a hack.  Figure out the right way.

# config <- config::get(config="../../config.yml")
# config <- config::get(config="config.yml")
# source("./scripts/common-functions.R")        # reporting functions and quick views
# source("./scripts/graphing/graph-presets.R") # font and color conventions
# source("./scripts/graphing/graph-support.R") # font and color conventions

ggplot2::theme_set(ggplot2::theme_bw())

ls <- readr::read_rds("data.rds")
ds_wide <- ls[["wide"]]
ds_long <- ls[["long"]]
available_metrics <-  unique(ds_long$metric)


library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Since Metrics"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId='var_name_x',
                label='Variable X',
                # choices=var_name_available,
                choices=available_metrics,
                selected=1
            ),
            selectInput(
                inputId='var_name_y',
                label='Variable Y',
                # choices=NULL,
                choices=available_metrics,
                # choices=unit_name_available,
                selected=2
            )#,

        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("metricPlot", height = 800, width = 1200)
            # plotOutput("distPlot"),
            # plotlyOutput("spaghetti_1")
            # plotlyOutput("spaghetti_bar_1", height = 800, width = 1200)
            # plotOutput("spaghetti_1")
            # plotOutput("test1")
        )
    )
))
