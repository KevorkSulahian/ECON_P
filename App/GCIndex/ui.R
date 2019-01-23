library(dplyr)
library(rpgm)
library(shiny)
library(shinydashboard)
library(fmsb)




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

# which(rownames(data18_rank) == "Armenia")

# data18_rank[which(rownames(data18_rank) == "Armenia"),]
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


country_pos <-5
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
  # print(star_pos(inflation, country_pos_all))
} else {
  data18AM[inflation, country_pos_this]  <- star_neg(inflation, country_pos_all)
  # print(star_pos(inflation, country_pos_all))
}

pillars_all<- data18[grep('pillar', data18$Series),]
pillars_all <- pillars_all[,-c(1:5)]
pillars_all <- as.matrix(pillars_all)
pillars_all2 <- pillars_all[,complete.cases(t(pillars_all))]
pillars_all2 <- as.data.frame(pillars_all2)
# using pillars_all2 rn
corr <- cor(pillars_all2)
res <- which(lower.tri(corr) >.3, arr.ind = T)
res2 <- res[res[,1] != res[,2],]
similars <- data.frame(res2, correlation = corr[res2])
similars <- similars[complete.cases(similars),]
similars <- similars[similars$row %in% 5 | similars$col %in% 5,]

# corr[27,]



group <- read.csv("data.csv")
group <- group[c(1,2)]
group<-group[-1,]
colnames(group) = c("Country", "GDP")
group$group <- 0
group <- group[complete.cases(group),]
group <- group[order(group$GDP, decreasing = T),]
rownames(group) <- NULL
group$GDP_Rank <- rownames(group)
for (i in 1:length(group$GDP)) {
  if (group$GDP[i] < 2000) {
    group$group[i] <- 1
  } else if(group$GDP[i] < 3000) {
    group$group[i] <- 2
  } else if (group$GDP[i]< 9000) {
    group$group[i] <- 3
  } else{
    group$group[i]<- 4
  }
}

group$Country <- as.character(group$Country)
group$Country[group$Country == "Iran"] <- "Iran, Islamic Rep."
group2 <- group
group <- group[group$group %in% group$group[group$Country %in% "Armenia"],]
## BIG BOI FUNCTIONS

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

# get_number("B.10", "Armenia")

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
  colnames(temp) <- c(data18$`Series unindented`[data18$`Code GCR` %in% id], "Country", "GCI Rank")
  temp$`Pillar Rank`<- rownames(temp)
  temp$`GCI Rank` <- as.character(temp$`GCI Rank`)
  return(temp[,c(2,1,4,3)])
}
# id = "GCI"
# aa <- get_table("")
get_table_GCI <- function() {
  temp <- data.frame(t(data18_countries[data18$`Code GCR` %in% "GCI",]), ordered = T)
  colnames(temp) <- c("V1", "V2")
  # temp$V1 <- as.numeric(temp$V1)
  temp <- as.data.frame(temp[complete.cases(temp),])
  
  temp$V2 <- rownames(temp)
  colnames(temp) <- c("V1", "Country")
  temp <- temp[temp$Country %in% rownames(data18_rank_cont),]
  temp$Rank <- data18_rank_cont[temp$Country %in% rownames(data18_rank_cont),]
  temp <- arrange(temp, desc(V1))
  colnames(temp) <- c("GCI", "Country", "Pillar_ Rank")
  temp$Pillar_Rank <- as.character(temp$Rank)
  temp2<- merge(temp, group2, by ="Country", all.x = T)
  return(temp2[,c(2,1,3,4,6)])
}


get_diff <- function(number) {
  data18_rank[which(rownames(data18_rank) == "Armenia"),] <- number
  data18_rank <- data18_rank[order(data18_rank$data18_rank, decreasing = T),  , drop = F]
  return(which(rownames(data18_rank) == "Armenia"))
}

rank <- 73

# get_diff(4.5)


## for part 2 now


set.seed(1)
# Mat <- matrix(rnorm(300), ncol = 10)
# combination <- combn(1:ncol(Mat), m = 2)
# combination <- as.data.frame(combination)
# combination <- combination[combination[1,] %in% 2 | combination[2,] %in% 2] #where 2 is armenia
# sigma <- NULL
# for (i in 1:ncol(combination)) {
#   sigma <- c(sigma, summary(lm(Mat[,combination[1,i]] ~ Mat[,combination[2,i]]))$sigma)
# }
# 
# sigma <- as.data.frame(sigma)
# 
# c(summary(lm(Mat[,combination[1,1]] ~ Mat[,combination[2,1]]))$sigma,
#   summary(lm(Mat[,combination[1,2]] ~ Mat[,combination[2,2]]))$sigma)
# 

pillars_all2 <- pillars_all[,complete.cases(t(pillars_all))]
pillars_all2 <- as.data.frame(pillars_all2)

country.names <- as.character(group$Country)
# pillars_all3 <- pillars_all2[country.names]


pillars_all3 <- pillars_all2 %>%
  select(c(country.names))

combination2 <- combn(1:ncol(pillars_all3), m = 2)
combination2 <- as.data.frame(combination2)
combination2 <- combination2[combination2[1,] %in% 28 | combination2[2,] %in% 28] #where 3 is armenia
colnames(combination2) <- c(1:ncol(combination2))
sigma2 <- NULL
for (i in 1:ncol(combination2)) {
  sigma2 <- c(sigma2, summary(lm(pillars_all3[,combination2[1,i]] ~ pillars_all3[,combination2[2,i]]))$sigma)
}

sigma2 <- as.data.frame(sigma2)
sigma2$location <- rownames(sigma2)
sigma2 <- sigma2[order(sigma2),]
sigma2 <- sigma2[complete.cases(sigma2),]
top_3 <- head(sigma2, n= 15)
top_3_comb <- combination2[top_3$location]

lenght_row = length(c(top_3_comb[top_3_comb[1,]%in% 28]))
length_column =  length(c(top_3_comb[top_3_comb[2,]%in% 28])) 
top_3_countries_index<- NULL

# row
for(i in 1:lenght_row) {
  top_3_countries_index <- c(top_3_countries_index, top_3_comb[top_3_comb[1,]%in% 28][[i]][2])
}
# col
for(i in 1:length_column) {
  top_3_countries_index <- c(top_3_countries_index, top_3_comb[top_3_comb[2,]%in% 28][[i]][1])
}
# this way it's ordered
top_3_countries <- pillars_all3[c(21,17,22,19,12,4,29,23,15,13,30,26,18,1,20)]

ui <- dashboardPage(
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
        
        '
      ))
      ),
    fluidRow(
      tabBox(id = "first", width = 12,
             tabPanel("GCI",
                      
                      fluidRow(
                        column(4,
                               fluidRow(selectInput("A.01", "Institutions", choices = colnames(data18_countries), selected = "Armenia")),
                               fluidRow(selectInput("A.02", "Infrastructure", choices = colnames(data18_countries), selected = "Armenia")),
                               fluidRow(selectInput("A.03", "Macroeconomic environment", choices = colnames(data18_countries), selected = "Armenia")),
                               fluidRow(selectInput("A.04", "Health and primary education", choices = colnames(data18_countries), selected = "Armenia")),
                               fluidRow(selectInput("B.05", "Higher education and training", choices = colnames(data18_countries), selected = "Armenia")),
                               fluidRow(selectInput("B.06", "Goods market efficiency", choices = colnames(data18_countries), selected = "Armenia"))),
                        
                        column(2,
                               fluidRow(h3(textOutput("A.01_out"))), 
                               fluidRow(h3(textOutput("A.02_out"))),
                               fluidRow(h3(textOutput("A.03_out"))),
                               fluidRow(h3(textOutput("A.04_out"))),
                               fluidRow(h3(textOutput("B.05_out"))),
                               fluidRow(h3(textOutput("B.06_out")))
                        ),
                        
                        column(4,
                               fluidRow(selectInput("B.07", "Labor market efficiency", choices = colnames(data18_countries), selected = "Armenia")),
                               fluidRow(selectInput("B.08", "Financial market development", choices = colnames(data18_countries), selected = "Armenia")),
                               fluidRow(selectInput("B.09", "Technological readiness", choices = colnames(data18_countries), selected = "Armenia")),
                               fluidRow(selectInput("B.10", "Market size", choices = colnames(data18_countries), selected = "Armenia")),
                               fluidRow(selectInput("C.11", "Business sophistication", choices = colnames(data18_countries), selected = "Armenia")),
                               fluidRow(selectInput("C.12", "Innovation", choices = colnames(data18_countries), selected = "Armenia"))),
                        
                        column(2,
                               fluidRow(h3(textOutput("B.07_out"))),
                               fluidRow(h3(textOutput("B.08_out"))),
                               fluidRow(h3(textOutput("B.09_out"))),
                               fluidRow(h3(textOutput("B.10_out"))),
                               fluidRow(h3(textOutput("C.11_out"))),
                               fluidRow(h3(textOutput("C.12_out")))   
                        )),
                      
                      fluidRow(
                        div(style = "text-align: center",h2("Closest countries by pillars"))
                        
                      ),
                      
                      fluidRow(
                        column(12,
                               tableOutput(outputId = "table2"))
                        
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
    )

rsconnect::setAccountInfo(name='kevorkysulahian', token='EFB83E28B0842C8B95B6B7F4052111FB', secret='pceBrEq/Ohn0CEYtuvNIg43WqRdxJd2cwLetcmOS')
library(rsconnect)
deployApp(appName = "GCIndex")
