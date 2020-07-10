#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(tsbox)
library(openair)
library(writexl)
library(gridExtra)
library(ggplot2)
library(dplyr)


get_GDP_quarter <- function() {
  
  df_growth <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTHV3LyouKv1E3XAgpnWDBY3aTUsz55MeYY6LJNp_AH3_Lz_ikjNX91A6W0Hq35IA/pub?gid=1836951832&single=true&output=csv",
                        stringsAsFactors = F)
  df_growth[c(3:8)] <- sapply(df_growth[c(3:8)],as.character)
  df_growth[c(3:8)] <- sapply(df_growth[c(3:8)],function(x) gsub( ",", "",as.character(x)))
  df_growth[c(3:8)] <- sapply(df_growth[c(3:8)],as.numeric)
  df_growth[c(3:8)] <- round(x = df_growth[c(3:8)],digits = 1)
  df_growth$type = "Growth (%)"
  df_absolute <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTHV3LyouKv1E3XAgpnWDBY3aTUsz55MeYY6LJNp_AH3_Lz_ikjNX91A6W0Hq35IA/pub?gid=2008348090&single=true&output=csv",
                          stringsAsFactors = F)
  df_absolute[c(3:8)] <- sapply(df_absolute[c(3:8)],as.character)
  df_absolute[c(3:8)] <- sapply(df_absolute[c(3:8)],function(x) gsub( ",", "",as.character(x)))
  df_absolute[c(3:8)] <- sapply(df_absolute[c(3:8)],as.numeric)
  df_absolute[c(3:8)] <- df_absolute[c(3:8)] / 1000
  df_absolute[c(3:8)] <- round(x = df_absolute[c(3:8)],digits = 1)
  df_absolute$type = "Absolute value (mil AMD)"
  return(list(df_growth,df_absolute,rbind(df_growth,df_absolute)))
}

get_GDP_yearly <- function() {
  df_growth <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSJxtlLM8CzXGQ9Eg0KBkOcOT-dO-ChUtQl9y-qvAtysQjwUAeuI7VwIKKlu6wz5Q/pub?gid=921778851&single=true&output=csv",
                        stringsAsFactors = F)
  df_growth[c(2:7)] <- sapply(df_growth[c(2:7)],as.character)
  df_growth[c(2:7)] <- sapply(df_growth[c(2:7)],function(x) gsub( ",", "",as.character(x)))
  df_growth[c(2:7)] <- sapply(df_growth[c(2:7)],as.numeric)
  df_growth[c(2:7)] <- df_growth[c(2:7)] - 100
  df_growth[c(2:7)] <- round(x = df_growth[c(2:7)],digits = 1)
  df_growth$type = "Growth (%)"
  df_absolute <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSJxtlLM8CzXGQ9Eg0KBkOcOT-dO-ChUtQl9y-qvAtysQjwUAeuI7VwIKKlu6wz5Q/pub?gid=170066381&single=true&output=csv",
                          stringsAsFactors = F)
  df_absolute[c(2:7)] <- sapply(df_absolute[c(2:7)],as.character)
  df_absolute[c(2:7)] <- sapply(df_absolute[c(2:7)],function(x) gsub( ",", "",as.character(x)))
  df_absolute[c(2:7)] <- sapply(df_absolute[c(2:7)],as.numeric)
  df_absolute[c(2:7)] <- df_absolute[c(2:7)] / 1000
  df_absolute[c(2:7)] <- round(x = df_absolute[c(2:7)],digits =1)
  df_absolute$type = "Absolute value (mil AMD)"
  return(list(df_growth,df_absolute,rbind(df_growth,df_absolute)))
}

get_export_import <- function() {
  df <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRnjDARR4PTnuOVB-4Wll3UgCEjghmOGBY2M1bxJQixX7PFgEO9GcoUmyN9g1-ziQR9-YB7B_Gp1z54/pub?gid=1397351662&single=true&output=csv",
                 stringsAsFactors = F)
  colnames(df) <- c("Commodity", "Codes", "Year", "Month","Export in Tons","Export in 1000 $",
                    "Import in Tons", "Import in 1000 $")
  df$Code_Commodity <- paste(df$Codes,df$Commodity)
  df <- df[c(9,3:8)]
  # df[1:3] <- sapply(df[1:3],as.factor)
  df[, c(1:3)] <- lapply(df[, c(1:3)], factor)
  
  ##
  
  df[,c(5,7)] <- lapply(df[, c(5,7)], round)
  df[,c(4,6)] <- lapply(df[, c(4,6)], round)
  return(df)
}


get_gdp_year <- function(df,years) {
  return(df[df$Year %in% years,])
}

get_quarter <- function(df,quarter) {
  return(df[df$Quarter %in% quarter,])
}

get_import_export_indicator <- function(df,indicator,type) {
  
  # print(indicator)
  if (length(indicator) == 2) {
    return(df[c(1:3,c(c(indicator[1] + type),c(indicator[2] + type)))])
  }
  else if(length(indicator) == 1){
    return(df[c(1:3,c(c(indicator+ type)))])
  }
  # else return("yeet")
}

get_import_export_indicator2 <- function(df,indicator,type) {
  
  # print(indicator)
  if (length(indicator) == 2) {
    return(df[c(1,c(c(indicator[1] + type),c(indicator[2] + type)))])
  }
  else if(length(indicator) == 1){
    return(df[c(1,c(c(indicator+ type)))])
  }
  # else return("yeet")
}


shinyServer(function(input, output) {
  data_quarter_main <- reactive({
    temp <- get_GDP_quarter()
  })

  data_quarter <- reactive({
    temp <- data_quarter_main()
    temp <- as.data.frame(temp[sum(as.numeric(input$quarter_type))])
    temp <- temp[order(temp$Year),]
    temp <- get_gdp_year(temp,input$gdp_quarter_year)
    temp <- get_quarter(temp, input$gdp_quarter_quarter)
    temp <- temp[c("Year", "Quarter", c(input$quarterly_indicator),"type")]
  })
  
  data_yearly_main <- reactive({
    temp <- get_GDP_yearly()
  })
  
  data_yearly <- reactive({
   temp <- data_yearly_main()
   temp <- as.data.frame(temp[sum(as.numeric(input$yearly_type))])
   temp <- temp[order(temp$Year),]
   temp <- get_gdp_year(temp,input$gdp_yearly_year)
   temp <- temp[c("Year",c(input$yearly_indicator),"type")]
  })
  
  data_export_import_main <- reactive({
    temp <- get_export_import()
  })
  
  data_export_import <- reactive({
    temp <- data_export_import_main()
    temp <- temp[temp$Code_Commodity %in% input$code_and_commodity,]
    temp <- temp[temp$Month %in% input$import_export_month,]
    
    return(get_import_export_indicator(temp,
                                      as.numeric(input$import_export_indicator_selector),
                                      as.numeric(input$import_export_indicator_selector_type)))

  })
  
  data_export_import2 <- reactive({
    temp <- data_export_import_main()
    temp = temp %>%
      group_by(Code_Commodity) %>%
      summarise(`Export in Tons` = sum(`Export in Tons`),
                `Export in 1000 $` = sum(`Export in 1000 $`),
                `Import in Tons` = sum(`Import in Tons`),
                `Import in 1000 $` = sum(`Import in 1000 $`))
    # print(paste("input$code_and_commodity2",input$code_and_commodity2))
    temp <- temp[temp$Code_Commodity %in% input$code_and_commodity2,]
    # print("what")
    return(get_import_export_indicator2(temp,
                                       as.numeric(input$import_export_indicator_selector2),
                                       as.numeric(input$import_export_indicator_selector_type2)))
    
  })
  
  output$ui_of_selector <- renderUI({
    temp <- data_export_import_main()
    selectInput('code_and_commodity','HS 4 digit code', temp$Code_Commodity, width = '100%',multiple = T)
  })
  
  output$ui_of_selector2 <- renderUI({
    temp <- data_export_import_main()
    selectInput('code_and_commodity2','HS 4 digit code', temp$Code_Commodity, width = '100%',multiple = T)
  })
  
  output$import_export_indicator1 <-renderUI({
    selectInput(inputId="import_export_indicator_selector", label = "indicator",
                choices = c("Export" = 4,
                            "Import" = 6),
                multiple = T,selected = 4)
  })
  
  output$import_export_indicator2 <-renderUI({
    selectInput(inputId="import_export_indicator_selector2", label = "indicator",
                choices = c("Export" = 2,
                            "Import" = 4),
                multiple = T,selected = 4)
  })

  output$mytable3_quarter = DT::renderDataTable(
    data_quarter(),rownames= FALSE, filter = "none",selection = 'none', options = list(
      pageLength = 10, info = FALSE,columnDefs = list(list(className = 'dt-center', targets = "_all")))
    
  )
  
  output$mytable3_yearly = DT::renderDataTable(
    data_yearly(),rownames= FALSE, filter = "none",selection = 'none', options = list(
      pageLength = 10, info = FALSE,columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
  )
  
  output$mytable_export = DT::renderDataTable(
    data_export_import(), rownames = F, filter ="none", selection="none", options = list(
      pageLength = 10, info = F, columnDefs = list(list(className = "dt-center", targets = "_all")),
      autoWidth = F
    )
  )
  
  output$mytable_export2 = DT::renderDataTable(
    data_export_import2(), rownames = F, filter ="none", selection="none", options = list(
      pageLength = 10, info = F, columnDefs = list(list(className = "dt-center", targets = "_all")),
      autoWidth = F
    )
  )
  
  # Downloadable csv of selected dataset ----
  output$downloadData1 = downloadHandler(
    filename = function() {
      ("data_quarterly.xlsx")
    },
    content = function(file) {
      write_xlsx(data_quarter(), file)
    }
  )
  
  output$downloadData2 = downloadHandler(
    filename = function() {
      return("data_yearly.xlsx")
    },
    content = function(file) {
      write_xlsx(data_yearly(), file)
    }
  )
  
  output$downloadData3 = downloadHandler(
    filename = function() {
      return("data.xlsx")
    },
    content = function(file) {
      write_xlsx(data_export_import(), file)
    }
  )
  
  output$downloadData4 = downloadHandler(
    filename = function() {
      return("data.xlsx")
    },
    content = function(file) {
      write_xlsx(data_export_import2(), file)
    }
  )
  
})