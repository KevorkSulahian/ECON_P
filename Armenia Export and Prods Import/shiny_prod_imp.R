library(stringr)
library(utf8)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(rsconnect)
library(dplyr)
library(writexl)
library(dplyr)
library(writexl)
library(stringr)
library(data.table)

runApp(
  list(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar( 
        selectInput(inputId = "year",label="Choose year",
                    choices=c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017")),
        
        
        actionButton(
          inputId = "submit_loc",
          label = "Submit"
        ),
        br(),
        br(),
        downloadButton("downloadData", "Download",class="butt1")
        # tags$head(tags$style(".butt1{background-color:blue;} .butt1{color: black;} .butt1{font-family: Courier New} ,butt1{align: center}"))
      ),
      
      dashboardBody(
        # fluidRow(
        # tags$hr(),
        dataTableOutput("table")
        
      ))
    ,
    server = shinyServer(function(input, output,session) {
      
      
      my_data <- reactive({
        
        main  <- readxl::read_xlsx("Trade_Map_-_List_of_exported_products_for_the_selected_product_(All_products).xlsx",
                                   sheet = "Trade_Map_-_List_of_exported_pr", col_names = TRUE) #Reading the file
        colnames(main)<- c('col1','col2') 
        main <- main[-c(1:8),]
        main$col1 <- substring(main$col1,2)
        ID <- as.data.frame(cbind(main$col1,main$col2))
        colnames(ID) <- c("Code",'Product label')
        ID$Code <- as.character(ID$Code)
        
        setwd("countries")
        files=list.files(pattern=".xlsx")
        setwd(for_main)
        
        Year=input$year
        no_data = c()
        for (i in 1:length(files)){
          
          path = files[i]
          country <-  readxl::read_xlsx(paste0("countries/",path),  sheet = 1, 
                                        col_names = FALSE) 
          country_name <- as.character(country[1, 'X__1'])
          country_name <- substring(country_name,30)
          # if (country_name == 'Armenia'){
          #   print(i)
          # }
          country <- country[complete.cases(country$X__2),]
          
          colnames(country) <- unlist(country[country$X__1 =='Code',])
          colnames(country) <- as.character(unlist(country[1,]))
          country = country[-1, ]
          country$Code <- substring(country$Code,2)
          cols <- colnames(country)
          name <- cols[grep(Year, cols)]
          if (length(name)!=0){
            test <- country[,c('Code',name)]
            
            colnames(test) <- c('Code',country_name )
            
          }
          else {
            test <- country[,'Code']
            test[country_name]=''
            no_data = c(no_data,country_name)
            
          }
          test$Code <- as.character(test$Code)
          ID <- left_join(ID,test,by='Code')
        }
        
        ID[is.na(ID)] <- 0
        ID[,no_data] = '-'
        
        char_cols <-c(no_data,'Code','Product label')
        ID[,!(names(ID) %in% char_cols)] <- apply(ID[,!(names(ID) %in% char_cols)], 2, as.numeric)
        options(scipen=999)
        part1 <- ID
        part1[,!(names(part1) %in% char_cols)]<- apply(part1[,!(names(part1) %in% char_cols)], 2, function(x) x/max(x))
        part1 <- part1[-1,]
        part1 <- part1[order(part1$Code),]
        test <- abs(part1$Argentina- part1$Armenia)/2
        
        part2<-part1
        part2[,!(names(part2) %in% char_cols)] <- apply(part2[,!(names(part2) %in% char_cols)],2,function(x) abs(x-part1$Armenia)/2) 
        TCIS <- apply(part2[,!(names(part2) %in% char_cols)],2,function(x) (1-sum(x))*100)
        
        tc <- data.frame(TCIS)
        tr_tc <- transpose(data.frame(TCIS))
        colnames(tr_tc)<-rownames(tc)
        tr_tc['Product label'] <- "TCI"
        tr_tc$Code <- NA
        tr_tc[no_data] = NA
        test3 <- rbind(tr_tc, part2)
        ord_col <- colnames(part1)
        test3 <- test3[,ord_col]
        test3$Armenia <- NULL
        
        
        return (test3)
      })
      
      
      observeEvent(
        eventExpr = input[["submit_loc"]],
        handlerExpr = {
          
          output$table <- renderDataTable({
            my_data()
          }) })
      output$downloadData <- downloadHandler(
        filename = function() {
          ("untitled.xlsx")
        },
        content = function(file) {
          write_xlsx(my_data(), file)
        }
      )
      
    })
  ))

