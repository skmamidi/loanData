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
  titlePanel("Home Loan data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       
     #  conditionalPanel(
    #       condition = "input.theTabs == 'marketShare'",
           uiOutput("reactCompetitors"),
           #submitButton("Submit")
       #),
       hr(),
       #conditionalPanel(
    #     condition = "input.theTabs == 'loan'",
    sliderInput("quantile","Number of Quantile Groups:",
                min = 1,max = 10,value = 4),
    hr(),
    sliderInput("bins", "Number of bins per quantile group:",
                       min = 10,max = 200,value = 50),
         hr(),    
     # ),
    uiOutput("reactStates"),
    hr(),
    submitButton("Redraw plot")
    ),
    
    mainPanel(
        tabsetPanel("theTabs", 
                    tabPanel("Documentation", htmlOutput("textDisplay")), #tabPanel("Documentation", textOutput("textDisplay")),
                    tabPanel("Market Share by State", plotlyOutput("marketShare"),value="marketShare"),
                    tabPanel("Loan Amount Distribution", plotlyOutput("histPlot"),value = "loan")
        )
    )
  )
))
