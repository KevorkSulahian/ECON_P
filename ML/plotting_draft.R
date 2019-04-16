library(sjmisc)
library(readxl)
library(treemap)
library(ggplot2)
library(ggraph)
library(igraph)
library(stringr)

df <- read_excel("try.xlsx")
df$iratsum = as.numeric(df$iratsum)
# df$iratsum
# class(df$iratsum)
df_letter <- df
df_letter$category <- ""

for (i in 1:nrow(df_letter)) {
  if(!is.na(str_extract(df$ID[i],'B'))) {
    df_letter$category[i] <- "B"
  } else if(!is.na(str_extract(df$ID[i],'C'))){
    df_letter$category[i] <- "C"
  } else if(!is.na(str_extract(df$ID[i],'D'))) {
    df_letter$category[i] <- "D"
  }
}
df_letter$category <- as.factor(df_letter$category)

<<<<<<< HEAD
{ 
=======
{
>>>>>>> 1ef6eb43b35a82610edbe211dbce2bd849d7b259
treemap(df_letter,index=c('name'),
        vSize='iratsum',
        vColor = 'category',
        type = 'categorical',
        palette = 'Set3',
        title = 'Something',
        fontsize.title = 14)
labelAlign= list(c('left','center'),c('left','top'), c('right','bottom'))
png(width = 1336,height=768, file='uspop_treemap')
}
