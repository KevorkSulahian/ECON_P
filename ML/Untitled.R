library(sjmisc)
library(readxl)
ydf <- read_excel("2019-2 (2).xls", sheet = "2019_Feb")

df <- ydf[,c(1,2)]
df <- df[-c(1:45),]

empty_rows(df)

df <- remove_empty_rows(df)
df$id="0"

colnames(df) <- c("name","rownumber","ID")
startsWith(df$name,"B")
df[startsWith(df$name,"B"),]

