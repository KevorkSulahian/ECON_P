library(sjmisc)
library(readxl)
library(treemap)
library(ggplot2)
library(ggraph)
library(igraph)
library(stringr)
library(RColorBrewer)

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

final <- data.frame()
for (i in 1:nrow(df_letter)) {
  if(nchar(df_letter$ID[i]) > 5) {
    final <- rbind(final, df_letter[i,])
  }
}

final$sub_category<- ""

for (i in 1:nrow(final)) {
  final$sub_category <- substr(final$ID,1,4)
}

{
png(width = 1336,height=768, file='uspop_treemap.png')
ploting <- treemap(final,index=c('category', 'name'),
        vSize='iratsum',
        # palette = ,
        vColor = 'category',
        type = 'categorical',
        # title = 'Something',
        overlap.labels = 0.7,
        fontsize.labels = 16
        # inflate.labels = T)
)
dev.off()
}


treemap(final,
        index=c("category","sub_category","name"),
        vSize="iratsum",
        type="index"
)

# writexl::write_xlsx(final, 'final.xlsx')
