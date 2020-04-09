#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
base::source("./scripts/available_labels.R")
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("COVID-19 Country Response"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId='var_name',
                label='Variable',
                choices=var_name_available,
                # choices=var_label_available,
                selected=1
            ),
            selectInput(
                inputId='unit_name',
                label='Unit Name',
                # choices=unit_label_available,
                choices=unit_name_available,
                selected=1
            )#,
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # plotOutput("distPlot"),
            # plotlyOutput("spaghetti_1")
            plotlyOutput("spaghetti_bar_1")
            # plotOutput("spaghetti_1")
        )
    )
))
