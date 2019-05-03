options(warn=-1)

library(dplyr)
# library(rpgm)
library(shiny)
library(shinydashboard)
library(fmsb)
library(stringr)
library(dplyr)
# library(rpgm)
library(knitr)
library(rmarkdown)
library(RJSONIO)
library(readxl)

# library(profvis)

# profvis({
#   
# })

runApp(
  list(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar(collapsed = T),
      dashboardBody(
        tags$head(
          tags$style(HTML(
            'h3 {height: 49px;}
            .btn {margin: 5px 10px;}
            .col-sm-4 {text-align: center;
            text-align: -webkit-center;}
            .col-sm-2 {text-align: center;
            text-align: -webkit-center;}
            .col-sm-6 {text-align: center;
            text-align: -webkit-center;}
            .col-sm-12 {text-align: center;
            text-align: -webkit-center;}
            #radar {min-height: 1000px;}
            #radar > img {width : 100%;
            height : 100%;}
            #table1{ font-size:16px; margin: 10px }
            #table2{ font-size:16px; margin: 10px }
            #table3{ font-size:16px; margin: 10px }
            
            '
          ))
          ),
        fluidRow(
          tabBox(id = "first", width = 12,
                 tabPanel("GCI",
                          
                          fluidRow(
                            column(4,
                                   fluidRow(selectInput("A.01", "Institutions", choices = "70 - Armenia")),
                                   fluidRow(selectInput("A.02", "Infrastructure", choices = "70 - Armenia")),
                                   fluidRow(selectInput("A.03", "Macroeconomic environment", choices = "70 - Armenia")),
                                   fluidRow(selectInput("A.04", "Health and primary education", choices = "70 - Armenia")),
                                   fluidRow(selectInput("B.05", "Higher education and training", choices = "70 - Armenia")),
                                   fluidRow(selectInput("B.06", "Goods market efficiency", choices = "70 - Armenia"))),
                            
                            column(2,
                                   fluidRow(h3(textOutput("A.01_out"))), 
                                   fluidRow(h3(textOutput("A.02_out"))),
                                   fluidRow(h3(textOutput("A.03_out"))),
                                   fluidRow(h3(textOutput("A.04_out"))),
                                   fluidRow(h3(textOutput("B.05_out"))),
                                   fluidRow(h3(textOutput("B.06_out")))
                            ),
                            
                            column(4,
                                   fluidRow(selectInput("B.07", "Labor market efficiency", choices ="70 - Armenia")),
                                   fluidRow(selectInput("B.08", "Financial market development", choices = "70 - Armenia")),
                                   fluidRow(selectInput("B.09", "Technological readiness", choices = "70 - Armenia")),
                                   fluidRow(selectInput("B.10", "Market size", choices = "70 - Armenia")),
                                   fluidRow(selectInput("C.11", "Business sophistication", choices = "70 - Armenia")),
                                   fluidRow(selectInput("C.12", "Innovation", choices = "70 - Armenia"))),
                            
                            column(2,
                                   fluidRow(h3(textOutput("B.07_out"))),
                                   fluidRow(h3(textOutput("B.08_out"))),
                                   fluidRow(h3(textOutput("B.09_out"))),
                                   fluidRow(h3(textOutput("B.10_out"))),
                                   fluidRow(h3(textOutput("C.11_out"))),
                                   fluidRow(h3(textOutput("C.12_out")))   
                            )),
                          
                          fluidRow(
                            div(style = "text-align: center",h2("Closest countries by pillars and GDP"))
                            
                          ),
                          
                          fluidRow(
                            column(12,
                                   tableOutput(outputId = "table2"))
                            
                          ),
                          
                          fluidRow(
                            div(style = "text-align: center",h2("Closest countries by pillars"))
                            
                          ),
                          
                          fluidRow(
                            column(12,
                                   tableOutput(outputId = "table3"))
                            
                          ),
                          
                          fluidRow(
                            column(4, actionButton(inputId = "calculate_gci", label = "calculate GCI"), offset = 4)
                          ),
                          
                          fluidRow(
                            div(
                              column(3, 
                                     div(h3("GCI"), h3(textOutput("GCI")))),
                              column(3, 
                                     div(h3("Difference"), h3(textOutput("GCI_diff")))),
                              column(3,
                                     div(h3("New rank"), h3(textOutput("GCI_rank")))),
                              column(3,
                                     div(h3("Rank difference"), h3(textOutput("rank_diff"))))
                              , align = "center")
                          ),
                          
                          fluidRow(
                            plotOutput("radar")
                          )
                 ),
                 
                 tabPanel("DATA",
                          actionButton("showTable1",label = "Institutions"),
                          actionButton("showTable2",label = "Infrastructure"),
                          actionButton("showTable3",label = "Macroeconomic environment"),
                          actionButton("showTable4",label = "Health and primary education"),
                          actionButton("showTable5",label = "Higher education and training"),
                          actionButton("showTable6",label = "Goods market efficiency"),
                          actionButton("showTable7",label = "Labor market efficiency"),
                          actionButton("showTable8",label = "Financial market development"),
                          actionButton("showTable9",label = "Technological readiness"),
                          actionButton("showTable10",label = "Market size"),
                          actionButton("showTable11",label = "Business sophistication"),
                          actionButton("showTable12",label = "Innovation"),
                          actionButton("showTableGCI",label = "GCI"),
                          tableOutput(outputId = "table1")
                 )
          )
          
        )        
          )
        ),
    server = shinyServer(function(input,output,session) {
      
      
      data18 <- read_xlsx("data18.xlsx")
      data18_countries <-read_xlsx("data18_countries.xlsx")
      data18_rank_cont <- read_xlsx("data18_rank_cont.xlsx")
      data18_rank <- read_xlsx("data18_rank.xlsx")
      data18AM <- read_xlsx("data18AM.xlsx")
      out <- read_xlsx("out.xlsx")
      out2 <- read_xlsx("out2.xlsx")
      group2 <- read_xlsx("group2.xlsx")
      
      rownames(data18_rank) <- data18_rank$Countries
      data18_rank$Countries <- NULL
      rownames(data18_rank_cont) <- data18_rank_cont$Countries
      data18_rank_cont$Countries <- NULL
      
      
      ### please 
      
      
      calculate_A <- function(data) {
        temp <- data %>%
          filter(grepl("^A", data18$`Code GCR`))
        temp <- temp %>%
          filter(nchar(temp$`Code GCR`) == 4)
        return((temp[[3]]))
      }
      
      calculate_B <- function(data) {
        temp <- data18AM %>%
          filter(grepl("^B", data18$`Code GCR`))
        temp <- temp %>%
          filter(nchar(temp$`Code GCR`) == 4)
        return(temp[[3]])
      }
      
      calculate_C <- function(data) {
        temp <- data %>%
          filter(grepl("^C", data18$`Code GCR`))
        temp <- temp %>%
          filter(nchar(temp$`Code GCR`) == 4)
        return((temp[[3]]))
      }
      A_pillars <- calculate_A(data18AM)
      B_pillars <- calculate_B(data18AM)
      C_pillars <- calculate_C(data18AM)
      
      test_A <- function(first, second, third, forth) {
        (mean(c(first,second,third,forth)))
      }
      
      test_B <- function(five, six, seven, eight, nine, ten) {
        (mean(c(five, six, seven, eight, nine, ten)))
      }
      
      test_C <- function(eleven, tweleve) {
        mean(c(eleven, tweleve))
      }
      
      A <- test_A(A_pillars[1], A_pillars[2], A_pillars[3], A_pillars[4])
      B <- test_B(B_pillars[1], B_pillars[2], B_pillars[3], B_pillars[4], B_pillars[5], B_pillars[6])
      C <- test_C(C_pillars[1], C_pillars[2])
      
      GCI <- function(A, B, C) {
        return(A*.4 + B*.5 + C*.1)
      }
      # 
      final <- GCI(A,B,C)
      
      get_number <- function(id, country) {
        temp <- data18 %>%
          filter(`Code GCR` %in% id) 
        temp <- temp %>%
          select(country)
        return(temp[[1]])
      }
      #
     # get_number("B.05", "Armenia")
      
      get_table <- function(id) {
        temp <- data.frame(t(data18_countries[data18$`Code GCR` %in% id,]), ordered = T)
        colnames(temp) <- c("V1", "V2")
        # temp$V1 <- as.numeric(temp$V1)
        temp <- temp[complete.cases(temp), , drop = F]
        
        temp$V2 <- rownames(temp)
        colnames(temp) <- c("V1", "Country")
        temp <- temp[temp$Country %in% rownames(data18_rank_cont),]
        rank <- (data18_rank_cont[temp$Country %in% rownames(data18_rank_cont),])
        # rank$data18_rank_cont
        temp$Rank <- rank$data18_rank_cont
        temp <- arrange(temp, desc(V1))
        colnames(temp) <- c(data18$`Series unindented`[data18$`Code GCR` %in% id], "Country", "GCI Rank")
        temp$`Pillar Rank`<- rownames(temp)
        temp$`GCI Rank` <- as.character(temp$`GCI Rank`)
        return(temp[,c(2,1,4,3)])
      }
      # id = "GCI"
      # aa <- get_table("A.01")
      get_table_GCI <- function() {
        temp <- data.frame(t(data18_countries[data18$`Code GCR` %in% "GCI",]), ordered = T)
        
        colnames(temp) <- c("V1", "V2")
        # temp$V1 <- as.numeric(temp$V1)
        temp <- as.data.frame(temp[complete.cases(temp),])
        
        temp$V2 <- rownames(temp)
        colnames(temp) <- c("V1", "Country")
        temp <- temp[temp$Country %in% rownames(data18_rank_cont),]
        rank <- (data18_rank_cont[temp$Country %in% rownames(data18_rank_cont),])
        # rank$data18_rank_cont
        temp$Rank <- rank$data18_rank_cont
        temp <- arrange(temp, desc(V1))
        colnames(temp) <- c("GCI", "Country", "Pillar_ Rank")
        temp$Pillar_Rank <- as.character(temp$`Pillar_ Rank`)
        temp2<- merge(temp, group2, by ="Country", all.x = T)
        return(temp2[,c(2,1,3,4,6)])
      }
      
       # get_table_GCI()
      get_diff <- function(number) {
        temp <- data18_rank
        
        temp[which(rownames(temp) == "Armenia"),] <- number
        temp$Country <- rownames(temp)
        temp <- temp[order(temp$data18_rank, decreasing = T),  , drop = F]
        return(which((temp$Country) == "Armenia"))
      }
      
      rank <- 73
      
      
      # here
      
      a1 <- get_table("A.01")
      a1 <- a1[c(1,3)]
      a1 <- paste0(a1$`Pillar Rank`, " - ", a1$Country)
      
      a2 <- get_table("A.02")
      a2 <- a2[c(1,3)]
      a2 <- paste0(a2$`Pillar Rank`, " - ", a2$Country)
      
      a3 <- get_table("A.03")
      a3 <- a3[c(1,3)]
      a3 <- paste0(a3$`Pillar Rank`, " - ", a3$Country)
      
      a4 <- get_table("A.04")
      a4 <- a4[c(1,3)]
      a4 <- paste0(a4$`Pillar Rank`, " - ", a4$Country)
      
      a5 <- get_table("B.05")
      a5 <- a5[c(1,3)]
      a5 <- paste0(a5$`Pillar Rank`, " - ", a5$Country)
      
      a6 <- get_table("B.06")
      a6 <- a6[c(1,3)]
      a6 <- paste0(a6$`Pillar Rank`, " - ", a6$Country)
      
      a7 <- get_table("B.07")
      a7 <- a7[c(1,3)]
      a7 <- paste0(a7$`Pillar Rank`, " - ", a7$Country)
      
      a8 <- get_table("B.08")
      a8 <- a8[c(1,3)]
      a8 <- paste0(a8$`Pillar Rank`, " - ", a8$Country)
      
      a9 <- get_table("B.09")
      a9 <- a9[c(1,3)]
      a9 <- paste0(a9$`Pillar Rank`, " - ", a9$Country)
      
      a10 <- get_table("B.10")
      a10 <- a10[c(1,3)]
      a10 <- paste0(a10$`Pillar Rank`, " - ", a10$Country)
      
      a11 <- get_table("C.11")
      a11 <- a11[c(1,3)]
      a11 <- paste0(a11$`Pillar Rank`, " - ", a11$Country)
      
      a12 <- get_table("C.12")
      a12 <- a12[c(1,3)]
      a12 <- paste0(a12$`Pillar Rank`, " - ", a12$Country)
      
      updateSelectInput(session,"A.01", "Institutions", choices = a1, selected = a1[55])
      updateSelectInput(session,"A.02", "Infrastructure", choices = a2, selected = a2[80])
      updateSelectInput(session,"A.03", "Macroeconomic environment", choices = a3, selected = a3[101])
      updateSelectInput(session,"A.04", "Health and primary education", choices = a4, selected = a4[55])
      updateSelectInput(session,"B.05", "Higher education and training", choices = a5, selected = a5[69])
      updateSelectInput(session,"B.06", "Goods market efficiency", choices = a6, selected = a6[35])
      updateSelectInput(session,"B.07", "Labor market efficiency", choices = a7, selected = a7[51])
      updateSelectInput(session,"B.08", "Financial market development", choices = a8, selected = a8[78])
      updateSelectInput(session,"B.09", "Technological readiness", choices = a9, selected = a9[77])
      updateSelectInput(session,"B.10", "Market size", choices = a10, selected = a10[115])
      updateSelectInput(session,"C.11", "Business sophistication", choices = a11, selected = a11[68])
      updateSelectInput(session,"C.12", "Innovation", choices = a12, selected = a12[70])
      
      ### please
      pill <- reactiveValues(temp1 = 0, temp2 = 0, temp3 = 0, temp4 = 0,
                             temp5 = 0, temp6 = 0, temp7 = 0, temp8 = 0,
                             temp9 = 0, temp10 = 0, temp11 = 0, temp12 = 0)
      
      
      # isolate(pill[['temp1']])
      

      observeEvent(input$A.01, {
        temp33 <- gsub(pattern = "[0-9]",x =   input$A.01, replacement = "")
        temp33 <- substring(temp33, 4)
        temp33 <- knitr::combine_words(temp33,and = "", sep = " ")
        output$A.01_out <-renderText(get_number("A.01", temp33))
        pill$temp1 <- (get_number("A.01", temp33))
      })
      observeEvent(input$A.02, {
        temp33 <- gsub(pattern = "[0-9]",x =   input$A.02, replacement = "")
        temp33 <- substring(temp33, 4)
        output$A.02_out <-renderText( get_number("A.02", temp33))
        pill$temp2 <- (get_number("A.02", temp33))
      })
      observeEvent(input$A.03, {
        temp33 <- gsub(pattern = "[0-9]",x =   input$A.03, replacement = "")
        temp33 <- substring(temp33, 4)
        output$A.03_out <-renderText( get_number("A.03", temp33))
        pill$temp3 <- (get_number("A.03", temp33))
      })
      observeEvent(input$A.04, {
        temp33 <- gsub(pattern = "[0-9]",x =   input$A.04, replacement = "")
        temp33 <- substring(temp33, 4)
        output$A.04_out <-renderText( get_number("A.04", temp33))
        pill$temp4 <- (get_number("A.04", temp33))
      })
      observeEvent(input$B.05, {
        temp33 <- gsub(pattern = "[0-9]",x = input$B.05, replacement = "")
        temp33 <- substring(temp33, 4)
        output$B.05_out <-renderText( get_number("B.05", temp33))
        pill$temp5 <- (get_number("B.05", temp33))
      })
      observeEvent(input$B.06, {
        temp33 <- gsub(pattern = "[0-9]",x = input$B.06, replacement = "")
        temp33 <- substring(temp33, 4)
        output$B.06_out <-renderText( get_number("B.06", temp33))
        pill$temp6 <- (get_number("B.06", temp33))
      })
      observeEvent(input$B.07, {
        temp33 <- gsub(pattern = "[0-9]",x = input$B.07, replacement = "")
        temp33 <- substring(temp33, 4)
        output$B.07_out <-renderText( get_number("B.07", temp33))
        pill$temp7 <- (get_number("B.07", temp33))
      })
      observeEvent(input$B.08, {
        temp33 <- gsub(pattern = "[0-9]",x = input$B.08, replacement = "")
        temp33 <- substring(temp33, 4)
        output$B.08_out <-renderText( get_number("B.08", temp33))
        pill$temp8 <- (get_number("B.08", temp33))
      })
      observeEvent(input$B.09, {
        temp33 <- gsub(pattern = "[0-9]",x =  input$B.09, replacement = "")
        temp33 <- substring(temp33, 4)
        output$B.09_out <-renderText( get_number("B.09", temp33))
        pill$temp9 <- (get_number("B.09", temp33))
      })
      observeEvent(input$B.10, {
        temp33 <- gsub(pattern = "[0-9]",x = input$B.10, replacement = "")
        temp33 <- substring(temp33, 4)
        output$B.10_out <-renderText( get_number("B.10", temp33))
        pill$temp10 <- (get_number("B.10", temp33))
      })
      observeEvent(input$C.11, {
        temp33 <- gsub(pattern = "[0-9]",x = input$C.11, replacement = "")
        temp33 <- substring(temp33, 4)
        output$C.11_out <-renderText( get_number("C.11", temp33))
        pill$temp11 <- (get_number("C.11", temp33))
      })
      observeEvent(input$C.12, {
        temp33 <- gsub(pattern = "[0-9]",x = input$C.12, replacement = "")
        temp33 <- substring(temp33, 4)
        output$C.12_out <-renderText( get_number("C.12", temp33))
        pill$temp12 <- (get_number("C.12", temp33))
      })
      
      observeEvent(input$showTable1, {
        temp <- get_table("A.01")
        
        output$table1 <- renderTable(temp)
      })
      observeEvent(input$showTable2, {
        temp <- get_table("A.02")
        output$table1 <- renderTable(temp)
      })
      observeEvent(input$showTable3, {
        temp <- get_table("A.03")
        output$table1 <- renderTable(temp)
      })
      observeEvent(input$showTable4, {
        temp <- get_table("A.04")
        output$table1 <- renderTable(temp)
      })
      observeEvent(input$showTable5, {
        temp <- get_table("B.05")
        output$table1 <- renderTable(temp)
      })
      observeEvent(input$showTable6, {
        temp <- get_table("B.06")
        output$table1 <- renderTable(temp)
      })
      
      observeEvent(input$showTable7, {
        temp <- get_table("B.07")
        output$table1 <- renderTable(temp)
      })
      
      observeEvent(input$showTable8, {
        temp <- get_table("B.08")
        output$table1 <- renderTable(temp)
      })
      
      observeEvent(input$showTable9, {
        temp <- get_table("B.09")
        output$table1 <- renderTable(temp)
      })
      
      observeEvent(input$showTable10, {
        temp <- get_table("B.10")
        output$table1 <- renderTable(temp)
      })
      
      observeEvent(input$showTable11, {
        temp <- get_table("C.11")
        output$table1 <- renderTable(temp)
      })
      
      observeEvent(input$showTable12, {
        temp <- get_table("C.12")
        output$table1 <- renderTable(temp)
      })
      
      observeEvent(input$showTableGCI, {
        temp <- get_table_GCI()
        output$table1 <- renderTable(temp)
      })
      
      observeEvent(input$calculate_gci, {
        
        tempA <- test_A(isolate(pill[["temp1"]]),isolate(pill[["temp2"]]),isolate(pill[["temp3"]]),isolate(pill[["temp4"]]))
        tempB <- test_B(isolate(pill[["temp5"]]),isolate(pill[["temp6"]]),isolate(pill[["temp7"]]),isolate(pill[["temp8"]]),
                        isolate(pill[["temp9"]]),isolate(pill[["temp10"]]))
        tempC <-test_C(isolate(pill[["temp11"]]),isolate(pill[["temp12"]]))
        
        output$GCI <- renderText(GCI(tempA, tempB, tempC))
        
        output$GCI_diff <- renderText(GCI(tempA, tempB, tempC) - final)
        
        output$GCI_rank <- renderText(get_diff(GCI(tempA, tempB, tempC)))
        
        output$rank_diff <- renderText(rank -get_diff(GCI(tempA, tempB, tempC)))
        
        # Radar function 1
        get_rada1 <- function() {
          # plot radar
          radar <- data18AM[grep('pillar', data18AM$Series),]
          radar$Series <- substr(x = radar$Series, start = 13, stop = 100)
          radar_p=as.data.frame(matrix( sample( 2:20 , 12 , replace=T) , ncol=12))
          colnames(radar_p)= radar$Series
          
          
          # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
          radar_p=rbind(rep(7,12) , rep(0,12) , radar_p)
          
          radar_p[4,] <- as.numeric(radar$Armenia)
          
          radar_p[3,] <-  c(isolate(pill[["temp1"]]),isolate(pill[["temp2"]]),isolate(pill[["temp3"]]),isolate(pill[["temp4"]]),
                            isolate(pill[["temp5"]]),isolate(pill[["temp6"]]),isolate(pill[["temp7"]]),isolate(pill[["temp8"]]),
                            isolate(pill[["temp9"]]),isolate(pill[["temp10"]]),isolate(pill[["temp11"]]),isolate(pill[["temp12"]]))
          # The default radar chart proposed by the library:
          
          color_borders = c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))
          colors_in = c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))
          output$radar <-renderPlot({radarchart(radar_p, axistype = 1,
                                                #custom polygon
                                                pcol = color_borders, pfcol = colors_in, plwd = 4, plty = 1,
                                                #custom grid
                                                cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,9,1), cglwd = 1,
                                                #custom labels
                                                vlcex = 1.5)
            legend(x = 0.8, y = 1.3, legend = c("New", "Old"), bty = "n", pch = 20, col = color_borders,
                   text.col = "black", cex = 1.5, pt.cex = 2)  
            
          })
        } # function ends here
        get_rada1()
      })
      ### tab 3 
      
      
      # get the data manually here too
      
      output$table2 <- renderTable(out, striped = T)
      
      
      output$table3 <- renderTable(out2, striped = T)
      
    })
    )
)
