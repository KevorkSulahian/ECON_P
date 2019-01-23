library(dplyr)
library(rpgm)
library(shiny)
library(shinydashboard)
library(fmsb)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  pill <- reactiveValues(temp1 = 0, temp2 = 0, temp3 = 0, temp4 = 0,
                         temp5 = 0, temp6 = 0, temp7 = 0, temp8 = 0,
                         temp9 = 0, temp10 = 0, temp11 = 0, temp12 = 0)
  
  
  # isolate(pill[['temp1']])
  
  observeEvent(input$A.01, {
    output$A.01_out <-renderText( get_number("A.01", input$A.01))
    
    pill$temp1 <- (get_number("A.01", input$A.01))
  })
  observeEvent(input$A.02, {
    output$A.02_out <- renderText(get_number("A.02", input$A.02))
    
    pill$temp2 <- (get_number("A.02", input$A.02))
  })
  observeEvent(input$A.03, {
    output$A.03_out <- renderText(get_number("A.03", input$A.03))
    pill$temp3 <- (get_number("A.03", input$A.03))
  })
  observeEvent(input$A.04, {
    output$A.04_out <- renderText(get_number("A.04", input$A.04))
    pill$temp4 <- (get_number("A.04", input$A.04))
  })
  observeEvent(input$B.05, {
    output$B.05_out <- renderText(get_number("B.05", input$B.05))
    pill$temp5 <- (get_number("B.05", input$B.05))
  })
  observeEvent(input$B.06, {
    output$B.06_out <- renderText(get_number("B.06", input$B.06))
    pill$temp6 <- (get_number("B.06", input$B.06))
  })
  observeEvent(input$B.07, {
    output$B.07_out <- renderText(get_number("B.07", input$B.07))
    pill$temp7 <- (get_number("B.07", input$B.07))
  })
  observeEvent(input$B.08, {
    output$B.08_out <- renderText(get_number("B.08", input$B.08))
    pill$temp8 <- (get_number("B.08", input$B.08))
  })
  observeEvent(input$B.09, {
    output$B.09_out <- renderText(get_number("B.09", input$B.09))
    pill$temp9 <- (get_number("B.09", input$B.09))
  })
  observeEvent(input$B.10, {
    output$B.10_out <- renderText(get_number("B.10", input$B.10))
    pill$temp10 <- (get_number("B.10", input$B.10))
  })
  observeEvent(input$C.11, {
    output$C.11_out <- renderText(get_number("C.11", input$C.11))
    pill$temp11 <- (get_number("C.11", input$C.11))
  })
  observeEvent(input$C.12, {
    output$C.12_out <- renderText(get_number("C.12", input$C.12))
    pill$temp12 <- (get_number("C.12", input$C.12))
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
  
  
  
  out <- as.data.frame(pillars_all3[c(28,top_3_countries_index)])
  output$table2 <- renderTable(out, striped = T)
}

# Run the application 


