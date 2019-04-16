library(readxl)
library(sjmisc)
library(stringr)
df <- read_excel("2019-2.xls", sheet = "2019_Փետրվար")

df <- df[,c(1,2,3,10)]
df <- df[-c(1:45),]
# empty_rows(df)

df <- remove_empty_rows(df)
df <- df[-c(319),]

df$ID="0"

colnames(df) <- c("rownumber","name","artank","iratsum", "ID")

group_pos <- grep('B|C|D',df$rownumber)
sub_group_pos <- grep('[0-9]{2}\\.', df$rownumber)
sub_sub_group_pos <- grep('[0-9]{2}\\.[0-9]+',df$rownumber)
sub_sub_group_pos<- sub_sub_group_pos[-3]
sub_group_pos <- sub_group_pos[!sub_group_pos %in% sub_sub_group_pos]
# sub_group_pos[24]
# str_extract(df$rownumber[292],'[0-9]{2}\\.[0-9]+')
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
    temper <- paste0(temp1,".",temp2)
    temp$ID[i] <- substr(temper,1,4)
  }
  else {
    temp$ID[i] <- paste0(temp1,".",temp2,j)
    j = j + 1
    }
}

for (i in 1:nrow(temp)) {
  if(nchar(temp$ID[i]) < 5) {
    temp$name[i] <- temp$rownumber[i]
  }
  
}

temp1 <- temp[complete.cases(temp),]
temp1$rownumber <- NULL

temp1<- temp1[,c(4,1,2,3)]
# writexl::write_xlsx(temp1,"try.xlsx")
