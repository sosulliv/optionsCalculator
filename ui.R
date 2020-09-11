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
    titlePanel("Options Calculator"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            width=5,
            selectInput(
                "optionType",
                label = h3("Option Type"),
                choices <- list("Call", "Put"),
            ),
            br(),
            searchInput(
                inputId = "search", label = h3("Search Ticker"),
                placeholder = "A placeholder",
                #btnSearch = icon("search"),
               # btnReset = icon("remove"),
                width = "450px",
                value = "SPY"
            ),
            br(),
            selectInput(
                
                "expDate",
                label = h3("Expiration Date"),
                choices <- list("2020-09-18",	"2020-10-16",	"2020-11-20",	"2020-12-18",	"2021-01-15",	"2021-03-19",	"2021-06-18",	"2021-09-17",	"2021-12-17",	"2022-01-21",	"2022-03-18",	"2022-06-17",	"2022-09-16",	"2022-12-16")#,
            ),
            uiOutput("contractList"),
            tableOutput("res"),
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            width=7,
            tableOutput("parameters"),
            plotOutput("plot1"),
            tableOutput("greeks")
        )
    )
))
