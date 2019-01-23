library(ggplot2)
library(scales)
library(lubridate)
library(dplyr)


df <- read.csv("multiTimeline.csv", header = F)

df <- df[-c(1,2),]

df$V1 <- as.Date(df$V1)
df$V2 <- as.numeric(df$V2)

ggplot(df, aes(x = df$V1, y = df$V2)) + geom_point()

ggplot(df, aes(x = df$V1, y = df$V2)) + geom_line()

ggplot(df, aes(x = df$V1, y = df$V2)) + geom_bar(stat = "identity")

by_month <- df %>% group_by(month=floor_date(V1, "month")) %>%
  summarize(amount=sum(V2))

ggplot(by_month, aes(x = by_month$month, y = by_month$amount, fill = by_month$month)) + geom_bar(stat = "identity")

ggplot(by_month, aes(x = by_month$month, y = by_month$amount, color = by_month$month)) + geom_line()

ggplot(by_month, aes(x = by_month$month, y = by_month$amount, color = by_month$month)) + geom_point()
