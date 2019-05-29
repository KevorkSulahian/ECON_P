library(readxl)
library(sjmisc)
library(stringr)

df <- read_excel("2017.xlsx", sheet = "2017 Դեկտեմբեր")

df <- df[-c(1:45),]

colnames(df) <- c("one","two","three", "four", "five","six", "seven", "eight", "nine", "ten", "eleven",
                  "tweleve", "thirteen", "fourtheen", "fifteen", "sixteen", "seventeen", "eighteen", 'nineteen', 'twenty')


df$one <- as.numeric(df$one)

df <- df[complete.cases(df[,1]),]
df$one <- NULL

df[,-1] <- sapply(df[,-1], as.numeric)
df <- df[complete.cases(df[,1]),]

writexl::write_xlsx(df, "ML_files/2017_DEC.xlsx")

rnorm(1,1,3)


