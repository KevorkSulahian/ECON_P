## app.R ##
library(shinydashboard)
library(stringr)
library(utf8)
library(ggplot2)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(rsconnect)
library(dplyr)
library(writexl)

library(shiny)
library("plotly")
## ui.R ##
sidebar <- dashboardSidebar(
  tabsetPanel(
    tabPanel("Map", fluid = TRUE,
            
               selectInput("Country", "Select Country", choices = "", selected = "")
               
             
    ),
    tabPanel("plot", fluid = TRUE,
               sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')
               
               )
             )
    )
  


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  sidebar,
  body
)
server <- function(input, output) {
  
}

shinyApp(ui, server)

