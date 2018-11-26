library(dplyr)
library(rpgm)
library(shiny)
library(shinydashboard)

data<- readxl::read_xlsx("data_main.xlsx", sheet = 2)

colnames(data) <- data[3,]

data <- data[-c(1:3),]
##### data18[-c(1:5)] do this to all not just countries
data18 <- data[data$Edition %in% "2017-2018",]
data18_rank <- data18[data18$Attribute %in% "Value",]
data18_rank_cont <- data18[data18$Attribute %in% "Rank",]
data18_rank_cont <- t(data18_rank_cont[data18_rank_cont$Series %in% "Global Competitiveness Index",])
data18_rank_cont <- data18_rank_cont[-c(1:8),]
data18_rank_cont <- data18_rank_cont[complete.cases(data18_rank_cont), drop = F]
data18_rank_cont <- as.data.frame(data18_rank_cont)
data18_rank_cont$data18_rank_cont <- as.numeric(as.character(data18_rank_cont$data18_rank_cont))

data18_rank <- t(data18_rank[data18_rank$Series %in% "Global Competitiveness Index",])
data18_rank <- data18_rank[-c(1:8),]
data18_rank <- as.data.frame(data18_rank)
row_names <- rownames(data18_rank)
row_names<- row_names[-c(which(is.na(data18_rank$data18_rank)))]
data18_rank <- data18_rank[-c(which(is.na(data18_rank$data18_rank))),]
data18_rank <- as.data.frame(data18_rank)
rownames(data18_rank) <- row_names

data18_rank <- data18_rank[order(data18_rank$data18_rank, decreasing = T),  , drop = F]


data18_rank <- data18_rank[rownames(data18_rank) %in% rownames(data18_rank_cont), , drop = F]
data18_rank$data18_rank <- as.numeric(as.character(data18_rank$data18_rank))

which(rownames(data18_rank) == "Armenia")

data18_rank[which(rownames(data18_rank) == "Armenia"),]
data18_rank_main <- data18_rank


data18 <- data18[data18$Attribute %in% "Value",]
data18$Dataset <- NULL
data18$`GLOBAL ID` <- NULL
data18$Placement <- NULL

data18[-c(1:5)] <- sapply(data18[-c(1:5)], as.numeric)

data18_countries <- data18[,-c(1:5)]


data18AM <- data18[c(2,3,10)]
for (i in 1:nrow(data18AM)) {
  if(nchar(data18AM$`Code GCR`[i]) > 10 | nchar(data18AM$`Code GCR`[i]) == 3) {
    data18AM$`Code GCR`[i] <- substr(data18AM$Series[i], 1,4)
  }
}

library(dplyr)
library(rpgm)

### make sure to make star pos
stars <- grep("\\*", data18AM$Series)
not_pos <- data18AM$Series[data18AM$`Code GCR` %in% c("4.03", "4.01", "4.05", "6.05","6.07", "7.04", "3.04", "3.03")]
star_name <- grep("\\*", data18AM$Series, value = T) # name not needed rn

for (i in 1:length(star_name)) {
  for (j in 1:length(not_pos)) {
    if(star_name[i] == not_pos[j]) {
      stars[i] = "d"
    }
  }
}

negative <- which(data18AM$`Code GCR` %in% c("4.03", "4.01", "4.05", "6.05","6.07", "7.04", "3.04"))

stars <- gsub(pattern = "d", replacement = "", x = stars)
stars <- as.numeric(stars)
stars <- stars[complete.cases(stars)]

country_pos_all <- grep("Armenia", colnames(data18_countries))
country_pos_this <- grep("Armenia", colnames(data18AM))
star_pos <- function(pos, country_pos) {
  max <- (apply(data18_countries[pos,], 1, max,na.rm = T))
  min <- apply(data18_countries[pos,], 1, min,na.rm = T)
  return (6 * ((data18_countries[pos, country_pos] - min) / (max - min))  + 1)
}

star_neg <- function(pos, country_pos) {
  max <- (apply(data18_countries[pos,], 1, max,na.rm = T))
  min <- apply(data18_countries[pos,], 1, min,na.rm = T)
  return (-6 * ((data18_countries[pos, country_pos] - min) / (max - min))  + 7)
}

for (i in 1:length(stars)) {
  temp <-star_pos(stars[i], country_pos)
  data18AM[stars[i], country_pos_this] <- temp
}  

for (i in 1:length(negative)) {
  temp <-star_neg(negative[i], country_pos)
  data18AM[negative[i], country_pos_this] <- temp
}  
inflation <-  which(data18AM$`Code GCR` %in% c("3.03"))

if((data18AM[inflation, country_pos_this] > 0.5) & (data18AM[inflation, country_pos_this] < 2.9)) {
  print(7)
  data18AM[inflation, country_pos_this] <- 7
} else if (data18AM[inflation, country_pos_this] < 0.5) {
  data18AM[inflation, country_pos_this]  <- star_pos(inflation, country_pos_all)
  print(star_pos(inflation, country_pos_all))
} else {
  data18AM[inflation, country_pos_this]  <- star_neg(inflation, country_pos_all)
  print(star_pos(inflation, country_pos_all))
}

# A <- data18AM %>%
#   filter(grepl("^A", data18AM$`Code GCR`))
# 
# 
# B <- data18AM %>%
#   filter(grepl("^B",data18AM$`Code GCR`))
# 
# C <- data18AM %>%
#   filter(grepl("^C", data18AM$`Code GCR`))
# 
# Pillar calculators
calculate_A <- function(data) {
  temp <- data %>%
    filter(grepl("^A", data$`Code GCR`))
  temp <- temp %>%
    filter(nchar(temp$`Code GCR`) == 4)
  return((temp[[3]]))
}

calculate_B <- function(data) {
  temp <- data %>%
    filter(grepl("^B", data$`Code GCR`))
  temp <- temp %>%
    filter(nchar(temp$`Code GCR`) == 4)
  return(temp[[3]])
}

calculate_C <- function(data) {
  temp <- data %>%
    filter(grepl("^C", data$`Code GCR`))
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

final <- GCI(A,B,C)

get_number <- function(id, country) {
  temp <- data18 %>%
    filter(`Code GCR` %in% id) 
  temp <- temp %>%
    select(country)
  return(temp[[1]])
}

get_number("B.10", "Armenia")

get_table <- function(id) {
  temp <- data.frame(t(data18_countries[data18$`Code GCR` %in% id,]), ordered = T)
  colnames(temp) <- c("V1", "V2")
  # temp$V1 <- as.numeric(temp$V1)
  temp <- as.data.frame(temp[complete.cases(temp),])
  temp$V2 <- rownames(temp)
  colnames(temp) <- c("V1", "Country")
  temp <- temp[temp$Country %in% rownames(data18_rank_cont),]
  temp$Rank <- data18_rank_cont[temp$Country %in% rownames(data18_rank_cont),]
  temp <- arrange(temp, desc(V1))
  colnames(temp) <- c(data18$`Series unindented`[data18$`Code GCR` %in% id], "Country", "Rank")
  return(temp[,c(2,1,3)])
}
id = "A.01"
aa <- get_table("A.01")

get_diff <- function(number) {
  data18_rank[which(rownames(data18_rank) == "Armenia"),] <- number
  data18_rank <- data18_rank[order(data18_rank$data18_rank, decreasing = T),  , drop = F]
  return(which(rownames(data18_rank) == "Armenia"))
}

get_diff(4.5)

runApp(
  list(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar(),
      dashboardBody(
        tags$head(
          tags$style(HTML(
            'h3 {height: 49px;}'
          ))
        ),
           fluidRow(
             tabBox(id = "first", width = 12,
                    tabPanel("tab1",
                  
                    fluidRow(
                      column(5, offset = 1,
                       fluidRow(selectInput("A.01", "Institutions", choices = colnames(data18_countries), selected = "Armenia")),
                       fluidRow(selectInput("A.02", "Infrastructure", choices = colnames(data18_countries), selected = "Armenia")),
                       fluidRow(selectInput("A.03", "Macroeconomic environment", choices = colnames(data18_countries), selected = "Armenia")),
                       fluidRow(selectInput("A.04", "Health and primary education", choices = colnames(data18_countries), selected = "Armenia")),
                       fluidRow(selectInput("B.05", "Higher education and training", choices = colnames(data18_countries), selected = "Armenia")),
                       fluidRow(selectInput("B.06", "Goods market efficiency", choices = colnames(data18_countries), selected = "Armenia")),
                       fluidRow(selectInput("B.07", "Labor market efficiency", choices = colnames(data18_countries), selected = "Armenia")),
                       fluidRow(selectInput("B.08", "Financial market development", choices = colnames(data18_countries), selected = "Armenia")),
                       fluidRow(selectInput("B.09", "Technological readiness", choices = colnames(data18_countries), selected = "Armenia")),
                       fluidRow(selectInput("B.10", "Market size", choices = colnames(data18_countries), selected = "Armenia")),
                       fluidRow(selectInput("C.11", "Business sophistication", choices = colnames(data18_countries), selected = "Armenia")),
                       fluidRow(selectInput("C.12", "Innovation", choices = colnames(data18_countries), selected = "Armenia"))),
                      
                      column(3,offset = 1,
                             fluidRow(h3(textOutput("A.01_out"))), 
                             fluidRow(h3(textOutput("A.02_out"))),
                             fluidRow(h3(textOutput("A.03_out"))),
                             fluidRow(h3(textOutput("A.04_out"))),
                             fluidRow(h3(textOutput("B.05_out"))),
                             fluidRow(h3(textOutput("B.06_out"))),
                             fluidRow(h3(textOutput("B.07_out"))),
                             fluidRow(h3(textOutput("B.08_out"))),
                             fluidRow(h3(textOutput("B.09_out"))),
                             fluidRow(h3(textOutput("B.10_out"))),
                             fluidRow(h3(textOutput("C.11_out"))),
                             fluidRow(h3(textOutput("C.12_out")))
                            )
                    ),
                    
                    fluidRow(
                      column(4, actionButton(inputId = "calculate_gci", label = "calculate GCI"), offset = 4)
                    ),
                    
                    fluidRow(
                      div(
                      column(3, 
                            div(h3("GCI"), h3(textOutput("GCI")), offset = 2)),
                      column(3, 
                            div(h3("difference"), h3(textOutput("GCI_diff")), offset = 1)),
                      column(3,
                             div(h3("new rank"), h3(textOutput("GCI_rank")), offset = 1))), align = "center")
                    ),
                    
                    tabPanel("tab2",
                             actionButton("showTable1",label = "1st pillar"),
                             actionButton("showTable2",label = "2nd pillar"),
                             actionButton("showTable3",label = "3rd pillar"),
                             actionButton("showTable4",label = "4th pillar"),
                             actionButton("showTable5",label = "5th pillar"),
                             actionButton("showTable6",label = "6th pillar"),
                             actionButton("showTable7",label = "7th pillar"),
                             actionButton("showTable8",label = "8th pillar"),
                             actionButton("showTable9",label = "9th pillar"),
                             actionButton("showTable10",label = "10th pillar"),
                             actionButton("showTable11",label = "11th pillar"),
                             actionButton("showTable12",label = "12th pillar"),
                             tableOutput(outputId = "table1")
                             ))
           
             )        
        )
    ),
    server = shinyServer(function(input,output,session) {
      
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
      observeEvent(input$calculate_gci, {

        tempA <- test_A(isolate(pill[["temp1"]]),isolate(pill[["temp2"]]),isolate(pill[["temp3"]]),isolate(pill[["temp4"]]))
        tempB <- test_B(isolate(pill[["temp5"]]),isolate(pill[["temp6"]]),isolate(pill[["temp7"]]),isolate(pill[["temp8"]]),
                        isolate(pill[["temp9"]]),isolate(pill[["temp10"]]))
        tempC <-test_C(isolate(pill[["temp11"]]),isolate(pill[["temp12"]]))
        
        output$GCI <- renderText(GCI(tempA, tempB, tempC))
        
        output$GCI_diff <- renderText(GCI(tempA, tempB, tempC) - final)
        
        output$GCI_rank <- renderText(get_diff(GCI(tempA, tempB, tempC)))
        
      })
    })
  )
)
  
