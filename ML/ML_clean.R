library(readxl)
library(sjmisc)
library(stringr)

df <- read_excel("2014.xlsx", sheet = "Դեկտեմբեր")

df <- df[-c(1:34),]
# df <- df[,c(1:23)]
colnames(df) <- c("one","two","three", "four", "five","six", "seven", "eight", "nine", "ten", "eleven",
                  "tweleve", "thirteen", "fourtheen", "fifteen", "sixteen")


df$one <- as.numeric(df$one)

df <- df[complete.cases(df[,1]),]
df$one <- NULL

df[,-1] <- sapply(df[,-1], as.numeric)
df <- df[complete.cases(df[,1]),]

writexl::write_xlsx(df, "ML_files/2014_DEC.xlsx")


