
library(ggplot2)
library(ggrepel)
library(ggthemes)

data <- readxl::read_xls("pruductivity countries 2017().xls", sheet = 2)
data1 <- readxl::read_xls("pruductivity countries 2017().xls", sheet = 3)


data<- data[c(1,2,7)]

data <- data[complete.cases(data),]

options(scipen = 999)

data <- data[data$Productivity <  90,]

df <- merge(data, data1, by = "Country", all.x = T)
df$GDP <- df$GDP/1000000
df <- df[c(1:4)]
colnames(df) <- c("Country","GDP", "Productivity", "Abb")
AM <- df[df$Country == "Armenia",]

ggplot(df, aes(x = GDP, y = Productivity)) + geom_point(color = "black", aes(size = Productivity), pch = 21) +
  scale_size(range = c(0,10)) +
  geom_text_repel(aes(GDP, Productivity, label = Abb)) +
  theme_economist_white() + 
  geom_point(data = AM, color = "red", size = 2.5) +
  xlim(c(0,3740232.44)) + ylim(c(0,90)) + coord_flip() + theme(legend.position="none") +
  labs(y = "ՀՆԱ (մլն ԱՄՆ դոլար)", x = "Արտադրողականություն (ԱՄՆ դոլար)")

ggplot(df, aes(y = Productivity, x = Abb)) + geom_bar(stat = 'identity') + coord_flip()
