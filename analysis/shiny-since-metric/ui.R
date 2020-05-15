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
