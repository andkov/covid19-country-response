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
    titlePanel("COVID-19 Votes"),

    sidebarLayout(
        sidebarPanel(
            dateRangeInput(
                inputId = "date1"
                ,label = "Chose Date Range"
                ,start = "2020-03-15"
                ,end = Sys.Date()
            )
        )
        ,mainPanel(plotOutput("plot1"))

    )


))
