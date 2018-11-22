library(dplyr)
library(rpgm)
library(shiny)
library(shinydashboard)

data<- readxl::read_xlsx("data_main.xlsx", sheet = 2)

colnames(data) <- data[3,]

data <- data[-c(1:3),]
##### data18[-c(1:5)] do this to all not just countries
data18 <- data[data$Edition %in% "2017-2018",]
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






runApp(
  list(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        numericInput(inputId = "sub_A", label = "sub A", value = A), 
        br(), br(),
        numericInput(inputId = "sub_B", label = "sub B", value = B),
        br(), br(), br(), br(), br(),
        numericInput(inputId = "sub_C", label = "sub C", value = C),
        br(),
        numericInput(inputId = "GCI", label = "GCI", value = final)
        
      ),
      dashboardBody(
        fluidRow(
          column(3, numericInput(inputId = "sub_A_1", label = "sub A 1", value = A_pillars[1])),
          column(3, numericInput(inputId = "sub_A_2", label = "sub A 2", value = A_pillars[2])),
          column(3, numericInput(inputId = "sub_A_3", label = "sub A 3", value = A_pillars[3])),
          column(3, numericInput(inputId = "sub_A_4", label = "sub A 4", value = A_pillars[4]))
        ),
        actionButton(inputId = "calculate_A", label = "calculate A"),
        br(), br(),
        fluidRow(
          column(4, numericInput(inputId = "sub_B_1", label = "sub B 1", value = B_pillars[1])),
          column(4, numericInput(inputId = "sub_B_2", label = "sub B 2", value = B_pillars[2])),
          column(4, numericInput(inputId = "sub_B_3", label = "sub B 3", value = B_pillars[3]))
          ),
        fluidRow(
          column(4, numericInput(inputId = "sub_B_4", label = "sub B 4", value = B_pillars[4])),
          column(4, numericInput(inputId = "sub_B_5", label = "sub B 5", value = B_pillars[5])),
          column(4, numericInput(inputId = "sub_B_6", label = "sub B 6", value = B_pillars[6]))
        ),
        actionButton(inputId = "calculate_B", label = "calculate B"),
        br(),br(),
        fluidRow(
          column(4, numericInput(inputId = "sub_C_1", label = "sub C 1", value = C_pillars[1])),
          column(4, numericInput(inputId = "sub_C_2", label = "sub C 2", value = C_pillars[2]))
        ),
        actionButton(inputId = "calculate_C", label = "calculate C"),
        br(),br(),
        actionButton(inputId = "calculate_gci", label = "calculate GCI")
        
        
      )
    ),
    server = shinyServer(function(input,output,session) {
      # my_data <- reactive({
      #   
      # })
      
      
      observeEvent(input$calculate_A, {
        
        updateNumericInput(session, "sub_A",
                           value = test_A(input$sub_A_1, input$sub_A_2, input$sub_A_3, input$sub_A_4))
      })
      
      observeEvent(input$calculate_B, {
        
        updateNumericInput(session, "sub_B",
                           value = test_B(input$sub_B_1, input$sub_B_2, input$sub_B_3, input$sub_B_4,
                                          input$sub_B_5, input$sub_B_6))
      })
      
      observeEvent(input$calculate_C, {
        
        updateNumericInput(session, "sub_C",
                           value = test_C(input$sub_C_1, input$sub_C_2))
      })
      
      observeEvent(input$calculate_gci, {
        updateNumericInput(session, "GCI",
                           value = GCI(input$sub_A, input$sub_B, input$sub_C))
      })
      
    })
  )
)
