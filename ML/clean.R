library(readxl)
library(sjmisc)
library(stringr)
df <- read_excel("2019-2.xls", sheet = "2019_Փետրվար")

df <- df[,c(1,2)]
df <- df[-c(1:45),]

empty_rows(df)

df <- remove_empty_rows(df)
df$id="0"

colnames(df) <- c("rownumber","name","ID")

group_pos <- grep('B|C|D',df$rownumber)
sub_group_pos <- grep('[0-9]{2}.', df$rownumber)
sub_sub_group_pos <- grep('[0-9]{2}.[0-9]+',df$rownumber)
sub_group_pos <- sub_group_pos[!sub_group_pos %in% sub_sub_group_pos]

temp <- df

for (i in 1:nrow(temp)) {
  if (i %in% group_pos) {
    print(paste("in group_pos", i))
  } else if(i %in% sub_group_pos) {
    print(paste("in sub_group_pos", i))
  }
  else {
    print("just a normal row")
  }
}
