library(readxl)
library(sjmisc)
library(stringr)

df <- read_excel("2019-6.xls", sheet = "2019_Հունիս ")

df <- df[-c(1:46),]
df <- df[,c(1:23)]
colnames(df) <- c("one","two","three", "four", "five","six", "seven", "eight", "nine", "ten", "eleven",
                  "tweleve", "thirteen", "fourtheen", "fifteen", "sixteen")


df$one <- as.numeric(df$one)

df <- df[complete.cases(df[,1]),]
df$one <- NULL

df[,-1] <- sapply(df[,-1], as.numeric)
df <- df[complete.cases(df[,1]),]

writexl::write_xlsx(df, "ML_files/2019_JUNE.xlsx")


