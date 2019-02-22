library(dplyr)
library(rpgm)
library(shiny)
library(shinydashboard)
library(fmsb)
library(stringr)
library(dplyr)
library(rpgm)
library(knitr)
library(rmarkdown)
library(RJSONIO)

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

country_pos <- grep("Armenia", colnames(data18_countries))
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
  data18AM[inflation, country_pos_this]  <- star_pos(inflation, country_pos)
  # print(star_pos(inflation, country_pos))
} else {
  data18AM[inflation, country_pos_this]  <- star_neg(inflation, country_pos)
  # print(star_pos(inflation, country_pos))
}

pillars_all<- data18[grep('pillar', data18$Series),]
pillars_all <- pillars_all[,-c(1:5)]
pillars_all <- as.matrix(pillars_all)

pillars_all2 <- pillars_all[,complete.cases(t(pillars_all))]
pillars_all2 <- as.data.frame(pillars_all2)


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


# stop here and get data's manually

## BIG BOI FUNCTIONS



## for part 2 now
### from now on delete the codes and just get the data

set.seed(1)

country.names <- as.character(group$Country)

### You can delete groups now

pillars_all3 <- pillars_all2 %>%
  select(c(country.names))

combination1 <- combn(1:ncol(pillars_all3), m = 2)
combination1 <- as.data.frame(combination1)
combination1 <- combination1[combination1[1,] %in% 28 | combination1[2,] %in% 28] #where 3 is armenia
colnames(combination1) <- c(1:ncol(combination1))
sigma1 <- NULL
for (i in 1:ncol(combination1)) {
  sigma1 <- c(sigma1, summary(lm(pillars_all3[,combination1[1,i]] ~ pillars_all3[,combination1[2,i]]))$sigma)
}

sigma1 <- as.data.frame(sigma1)
sigma1$location <- rownames(sigma1)
sigma1 <- sigma1[order(sigma1),]
sigma1 <- sigma1[complete.cases(sigma1),]
top_15 <- head(sigma1, n= 15)
top_15_comb <- combination1[top_15$location]

lenght_row = length(c(top_15_comb[top_15_comb[1,]%in% 28]))
length_column =  length(c(top_15_comb[top_15_comb[2,]%in% 28])) 
top_15_countries_index<- NULL

# row
for(i in 1:lenght_row) {
  top_15_countries_index <- c(top_15_countries_index, top_15_comb[top_15_comb[1,]%in% 28][[i]][2])
}
# col
for(i in 1:length_column) {
  top_15_countries_index <- c(top_15_countries_index, top_15_comb[top_15_comb[2,]%in% 28][[i]][1])
}

# this way it's ordered
top_15_countries  <- pillars_all3[top_15_countries_index]
# top_3_countries <- pillars_all3[c(21,17,22,19,12,4,29,23,15,13,30,26,18,1,20)]



### non grouped


combination2 <- combn(1:144, m = 2)
combination2 <- as.data.frame(combination2)
combination2 <- combination2[combination2[1,] %in% 4 | combination2[2,] %in% 4] #where 3 is armenia
colnames(combination2) <- c(1:ncol(combination2))
sigma2 <- NULL
for (i in 1:ncol(combination2)) {
  sigma2 <- c(sigma2, summary(lm(pillars_all2[,combination2[1,i]] ~ pillars_all2[,combination2[2,i]]))$sigma)
}

sigma2 <- as.data.frame(sigma2)
sigma2$location <- rownames(sigma2)
sigma2 <- sigma2[order(sigma2),]
sigma2 <- sigma2[complete.cases(sigma2),]
rownames(sigma2) = NULL
top_3 <- head(sigma2, n= 10)
top_3_comb <- combination2[top_3$location]

lenght_row = length(c(top_3_comb[top_3_comb[1,]%in% 4]))
length_column =  length(c(top_3_comb[top_3_comb[2,]%in% 4])) 
top_3_countries_index<- NULL

# row
for(i in 1:lenght_row) {
  top_3_countries_index <- c(top_3_countries_index, top_3_comb[top_3_comb[1,]%in% 4][[i]][2])
}
# col
for(i in 1:length_column) {
  top_3_countries_index <- c(top_3_countries_index, top_3_comb[top_3_comb[2,]%in% 4][[i]][1])
}

non_grouped_countries <- pillars_all2[,top_3_countries_index]

out <- as.data.frame(pillars_all3[c(28,top_15_countries_index)])
out2 <- as.data.frame(pillars_all2[c(4,top_3_countries_index)])


data18_rank$Countries <- rownames(data18_rank)
data18_rank_cont$Countries <- rownames(data18_rank_cont)

## start from here
# 
# write.csv(data18, "data18.csv", row.names = F)
# write.csv(data18_countries, "data18_countries.csv", row.names = F)
# write.csv(data18_rank_cont, "data18_rank_cont.csv", row.names = F)
# write.csv(data18_rank, "data18_rank.csv", row.names = F)
# write.csv(out, "out.csv", row.names = F)
# write.csv(out2, "out2.csv", row.names = F)
# write.csv(data18AM, "data18AM.csv", row.names = F)

writexl::write_xlsx(data18, "data18.xlsx")
writexl::write_xlsx(data18_countries, "data18_countries.xlsx")
data18_rank$Countries <- rownames(data18_rank)
writexl::write_xlsx(data18_rank_cont, "data18_rank_cont.xlsx")
writexl::write_xlsx(data18_rank, "data18_rank.xlsx")
writexl::write_xlsx(out, "out.xlsx")
writexl::write_xlsx(out2, "out2.xlsx")
writexl::write_xlsx(data18AM, "data18AM.xlsx")
writexl::write_xlsx(group2, "group2.xlsx")
