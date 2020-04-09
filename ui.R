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
                choices=c(
                    "CARECPRA", "CARECPRO", "DNSTDENT", "DNSTLPDN", "DNSTPADN",
                    "EMPLGENE", "EMPLGENP", "EMPLGYNE", "EMPLOTGP", "EMPLOTPH", "EMPLOTSP",
                    "EMPLPEDI", "EMPLPSYS", "EMPLSPEC", "EMPLSPMP", "EMPLSURG", "GEHETHSM",
                    "HEDUAPGR", "HEDUDNGR", "HEDUMEGR", "HEDUMWGR", "HEDUNUGR", "HEDUPHGR",
                    "HEDUPNGR", "HOEMHASN", "HOEMHEMP", "HOEMHHCA", "HOEMHNUR", "HOEMHOTH",
                    "HOEMHOTS", "HOEMHPHY", "HOPIFROH", "HOPILICS", "HOPILIMM", "HOPILIPS",
                    "HOPINROH", "HOPIOBED", "HOPIREHA", "HOPITBED", "HOPITPOH", "HOSPFRHO",
                    "HOSPGHOS", "HOSPNPHO", "HOSPPUHO", "HOSPTHOS", "IPINGACA", "IPINGAMA",
                    "IPINGAMH", "IPINMAMA", "IPINMAMH", "IPINMAMO", "IPINMRIA", "IPINMRIH",
                    "IPINMRIM", "IPINPETA", "IPINPETH", "IPINPETS", "IPINRTEA", "IPINRTEH",
                    "IPINRTEQ", "IPINSCAA", "IPINSCAH", "IPINSCAN", "MIDWMIDW", "MIDWMILP",
                    "MIDWMIPA", "MINUASSO", "MINUINFI", "MINULPAP", "MINULPPN", "MINUNULP",
                    "MINUPAAP", "MINUPANU", "MINUPAPN", "MINUQUAL", "PAGGF344", "PAGGF454",
                    "PAGGF564", "PAGGF65O", "PAGGF75O", "PAGGFEMM", "PAGGFU35", "PAGGHOMM",
                    "PAGGM344", "PAGGM454", "PAGGM564", "PAGGM65O", "PAGGM75O", "PAGGMU35",
                    "PAGGT344", "PAGGT454", "PAGGT564", "PAGGT65O", "PAGGT75O", "PAGGTOPY",
                    "PAGGTU35", "PHSTLPPH", "PHSTPAPH", "PHSTPHAR", "PHYSMEDE", "PHYSPAPS",
                    "PHYSREGP", "PSIOPHTH", "RVNURINF", "RVNURMED", "RVNURMEG"
                ),
                selected=1
            ),
            selectInput(
                inputId='unit_name',
                label='Unit Name',
                choices=c(
                    "DENSPPNB", "PERSMYNB", "PERMEDNB", "MILBIRNB", "PEREMPNB",
                    "NOMBRENB", "RTOALLNB", "PERPHYNB", "DENSEFEF", "FTEEMPEF", "PFHEMPEF",
                    "PTHEMPNB", "RTOINPNB", "LTCOVRNB", "HOPFTE", "HOPRAT", "NURFTE",
                    "NURRAT", "NBMILPNB", "PHY344NB", "PHY454NB", "PHY564NB", "PHYT65NB",
                    "PHYT75NB", "PHYTOTNB", "PHYU35NB", "YSALARMT", "YSALAWMT", "YSALGDMT",
                    "YSALPIMT", "YSALPPMT", "YSALXRMT", "YSEFAWMT", "YSEFGDMB", "YSEFPIMT",
                    "YSEFPPMB", "YSEFXRMB", "YSELFPMB"
                ),
                selected=1
            ),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
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
