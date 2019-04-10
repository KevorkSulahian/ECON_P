library(readxl)
library(sjmisc)
df <- read_excel("2019-2.xls", sheet = "2019_Փետրվար")

df <- df[,c(1,2)]
df <- df[-c(1:45),]

empty_rows(df)

df <- remove_empty_rows(df)
df$id="0"

colnames(df) <- c("rownumber","name","ID")
