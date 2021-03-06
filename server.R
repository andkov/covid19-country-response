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

# setwd("../..") # This is a hack.  Figure out the right way.

# config <- config::get(config="../../config.yml")
config <- config::get(config="config.yml")
source("./scripts/common-functions.R")        # reporting functions and quick views
# source("./scripts/graphing/graph-presets.R") # font and color conventions
# source("./scripts/graphing/graph-support.R") # font and color conventions

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

# ls_input_health$health_resources %>%
#     get_var_unit_lookup() %>%
#     # pull(VAR) %>%
#     pull(UNIT) %>%
#     unique() %>%
#     dput()

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
shinyServer(function(session, input, output) {

    # observe({
    #     variable_name <-
    # })
    # output$var_name <- renderText({
    #     paste("You chose", input$var_name)
    # })
    #
    # slidersLayout <- reactive({
    #     return(list(
    #         var_name = (input$var_name),
    #         unit_name = (input$unit_name)
    #     ))
    # })

    observe({
        v_var_names <- ls_input_health$health_resources %>% get_var_unit_lookup() %>%
            dplyr::distinct(var_label) %>% dplyr::pull()
        updateSelectInput(session,"var_name", "Choose Variable", choices = v_var_names, selected = v_var_names[1])
    })

    observe({
        v_unit_names <- ls_input_health$health_resources %>% get_var_unit_lookup() %>%
            dplyr::filter(var_label == input$var_name) %>% dplyr::distinct(unit_label) %>% dplyr::pull()
        updateSelectInput(session,"unit_name", "Choose Metric", choices = v_unit_names, selected = v_unit_names[1])
    })

    # var_name    <- c("HOPITBED", "HOPITBED2")[1]
    # unit_name   <- "RTOINPNB"
    # var_name_slider <- slidersLayout()$var_name
    # unit_name_slider <- slidersLayout()$unit_name

    # prep_data_trajectory <- function(ls_oecd, df_covid, n_deaths_first_day = 1, var_name, unit_name){
    prep_data_trajectory <- function(ls_oecd, df_covid, n_deaths_first_day = 1, var_label_i, unit_label_i){
        # browser()
        # var_name <- slidersLayout()$var_name
        # unit_name <- slidersLayout()$unit_name
        var_name <- input$var_name
        unit_name <- input$unit_name
        d_covid <- compute_epi_timeline(df_covid, n_deaths_first_day = n_deaths_first_day)#, d_country = ds_country)
        # d_oecd  <- ls_oecd %>% compute_rank(var_name = var_name, unit_name =unit_name)
        d_oecd  <- ls_oecd %>% compute_rank(var_label_i = var_name, unit_label_i =unit_name)
        d_out <-   dplyr::left_join(
            d_covid, d_oecd, by = c("country_code" = "COU")
        )
        return(d_out)
    }

    # to retrieve labels
    # var_ref_table <- ls_input_health$health_resources %>% get_var_unit_lookup()
    # var_label_available <- var_ref_table %>% dplyr::distinct(var_label) %>% dplyr::pull(var_label)
    # var_label_i <- "Practising caring personnel"
    # unit_label_available <- var_ref_table %>% dplyr::filter(var_label == var_label_i) %>%
    #     dplyr::pull(unit_label)
    # unit_label_i <- "Number of persons (head counts)"
    # creating dynamic inputs:
    # https://mastering-shiny.org/action-dynamic.html

    output$test1 <- renderPlot({

        var_name <- input$var_name
        unit_name <- input$unit_name
        # var_name <- slidersLayout()$var_name
        # unit_name <- slidersLayout()$unit_name

        d_measure <- prep_data_trajectory(
            ls_oecd = ls_input_health$health_resources
            ,df_covid = ds_covid
            ,n_deaths_first_day = 1
            # ,var_name = var_name
            # ,unit_name = unit_name
            ,var_label_i = var_name
            ,unit_label_i = unit_name
        )

        # title_top <- paste0(var_name, "---", unit_name)
        d_measure %>%
            dplyr::filter(var_label == var_name, unit_label == unit_name) %>%
            ggplot(aes(x = country, y = value))+
            geom_col()+
            labs(
                title = paste0(var_name," - ", unit_name)
            )

    })

    output$spaghetti_bar_1 <- renderPlotly({
    # output$spaghetti_1 <- renderPlotly({
    # output$spaghetti_1 <- renderPlot({

        var_name <- input$var_name
        unit_name <- input$unit_name

        d_measure <- prep_data_trajectory(
            ls_oecd = ls_input_health$health_resources
            ,df_covid = ds_covid
            ,n_deaths_first_day = 1
            # ,var_name = var_name
            # ,unit_name = unit_name
            ,var_label_i = var_name
            ,unit_label_i = unit_name
        )
        # browser()

        # spaghetti 1
        g1 <- d_measure %>%
            # dplyr::filter(epi_timeline <=30) %>%
            # ggplot(aes(x = epi_timeline, y = n_deaths, color = rank_percentile)) +
            ggplot(aes(x = epi_timeline, y = n_deaths, color = factor(n_tile))) +
            geom_line(aes(group = country_code)) +
            theme_minimal()+
            facet_wrap(~n_tile)+
            geom_smooth(aes(x = epi_timeline, y = n_deaths, group = 1), inherit.aes=F, method = "loess", color = "gray70") +
            labs(
                title = paste0(var_name," - ", unit_name)
            )
        spaghetti_1 <- plotly::ggplotly(g1)

        # bar 1
        d_rank <- d_measure %>%
            dplyr::distinct(country, VAR, UNIT, var_label, unit_label,
                            value, rank, n_tile)

        country_order <- d_rank %>%
            dplyr::arrange(rank) %>%
            pull(country)

        g2 <- d_rank %>%
            dplyr::mutate(
                country = factor(country,levels = country_order)
            ) %>%
            ggplot(aes(x = country, y = value, fill = factor(n_tile)))+
            geom_col()+
            geom_text(aes(label = round(value,1) ))+
            coord_flip()+
            theme_minimal()+
            labs(
                x = ""
                ,y = paste0(var_name," - ", unit_name)
            )
        # g
        bar_1 <- plotly::ggplotly(g2)

        spaghetti_bar_1 <- plotly::subplot(spaghetti_1, bar_1,nrows = 2)

        # spaghetti_1 <- g
    })



})
