#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("COVID-19 Votes")


    ,fluidRow(
            column(4,
                dateRangeInput(
                inputId = "date1"
                ,label = "Chose Date Range"
                ,start = "2020-03-15"
                ,end = Sys.Date()
                ,min = "2020-03-15"
            ))
            ,column(4,
                selectInput(
                inputId = "grouping"
                ,label = "Grouping"
                ,choices = c(
                    "region"
                    ,"winner_2016"
                    ,"governor_political_affiliation"
                    ,"state_senate_majority_political_affiliation"
                    ,"state_house_majority_political_affiliation"
                    ,"state_attorney_general_political_affiliation"
                    ,"state_leadership"
                    )
                )
                ,downloadButton(
                    "downloadplot"
                    ,label = "Download Plot"
                )
                )
            ,column(4,
                    selectInput(
                        inputId = "xaxis"
                        ,label = "Choose X-Axis"
                        ,choices = c(
                            "Cases (7-day average)"
                            ,"Cases (7DA/100K)"
                            ,"Cases (cumulative)"
                            ,"Cases (cum/100K)"
                            ,"Deaths (7-day average)"
                            ,"Deaths (7DA/100K)"
                            ,"Deaths (cumulative)"
                            ,"Deaths (cum/100K)"
                            ,"Tests (7-day average)"
                            ,"Tests (7DA/100K)"
                            ,"Tests (cumulative)"
                            ,"Tests (cum/100K)"
                        )
                        ,selected = "Cases (7DA/100K)"
                    )
                    ,selectInput(
                        inputId = "yaxis"
                        ,label = "Choose Y-Axis"
                        ,choices = c(
                            "Cases (7-day average)"
                            ,"Cases (7DA/100K)"
                            ,"Cases (cumulative)"
                            ,"Cases (cum/100K)"
                            ,"Deaths (7-day average)"
                            ,"Deaths (7DA/100K)"
                            ,"Deaths (cumulative)"
                            ,"Deaths (cum/100K)"
                            ,"Tests (7-day average)"
                            ,"Tests (7DA/100K)"
                            ,"Tests (cumulative)"
                            ,"Tests (cum/100K)"
                        )
                        ,selected = "Cases (cum/100K)"
                    )
                    )

    )

    ,plotOutput("plot1", height = "1000px")


))
