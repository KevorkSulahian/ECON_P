library(readxl)
library(sjmisc)
library(stringr)
df <- read_excel("2019-2.xls", sheet = "2019_Փետրվար")

df <- df[,c(1,2)]
df <- df[-c(1:45),]
df <- df[-313,]
empty_rows(df)

df <- remove_empty_rows(df)
df$ID="0"

colnames(df) <- c("rownumber","name","ID")

group_pos <- grep('B|C|D',df$rownumber)
sub_group_pos <- grep('[0-9]{2}\\.', df$rownumber)
sub_sub_group_pos <- grep('[0-9]{2}\\.[0-9]+',df$rownumber)
sub_group_pos <- sub_group_pos[!sub_group_pos %in% sub_sub_group_pos]

temp <- df
j = 1
for (i in 1:nrow(temp)) {
  
  if (i %in% group_pos) {
    temp1 <-str_extract(df$rownumber[i],'B|C|D')
    temp$ID[i] = temp1
    j = 1
  } else if(i %in% sub_group_pos) {
    temp2 <- str_extract(df$rownumber[i],'[0-9]{2}\\.')
    j = 1
    temp$ID[i] <- paste0(temp1,temp2)
  }
  else {
    temp$ID[i] <- paste0(temp1,".",temp2,j)
    j = j + 1
    }
}
