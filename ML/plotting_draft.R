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
df$artank = as.numeric(df$artank)
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

# final2 <- data.frame()
# for (i in 1:nrow(df_letter)) {
#   if(nchar(df_letter$ID[i]) == 4) {
#     final2 <- rbind(final2, df_letter[i,])
#   }
# }

final$sub_category<- ""

for (i in 1:nrow(final)) {
  final$sub_category <- substr(final$ID,1,4)
}

final$name <- paste(final$name, final$artank)

{
png(width = 1336,height=768, file='Company.png')
ploting <- treemap(final,index=c('category', 'name'),
        vSize='artank',
        palette = c("#54A7AA", "#B23D6D", "#F3CF24"),
        vColor = 'category',
        type = 'categorical',
        title = 'Something',
        position.legend = 'bottom',
        fontsize.labels = c(0,28),
        # inflate.labels = T,
        force.print.labels = F,
        overlap.labels = 0
)
dev.off()
}

# #54A7AA, #B23D6D, #F3CF24
# treemap(Jan_Feb_Data,
#         index=c("category", ),
#         vSize="",
#         type="index"
# )

Jan_Feb_Data <- readxl::read_excel('Jan_Feb_Data.xlsx')

Jan_Feb_Data$`Հունվար-Փետրվար`

Jan_Feb_Data$`Ընթացիկ գներով հունվար-փետրվար (մլրդ դրամ)`<-  paste0(Jan_Feb_Data$`Ընթացիկ գներով հունվար-փետրվար (մլրդ դրամ)`,
                                                                    " ", round(Jan_Feb_Data$`Հունվար-Փետրվար`))

{
  png(width = 1336,height=768, file='Sector2.png')
  ploting <- treemap(Jan_Feb_Data,index=c('category', "Ընթացիկ գներով հունվար-փետրվար (մլրդ դրամ)"),
                     vSize='Հունվար-Փետրվար',
                     palette = c("#54A7AA", "#B23D6D", "#F3CF24"),
                     vColor = 'category',
                     type = 'categorical',
                     title = 'Something',
                     position.legend = 'bottom',
                     fontsize.labels = c(0,26),
                     # inflate.labels = T,
                     force.print.labels = F,
                     overlap.labels = 0
  )
  dev.off()
}
 
# treemap
# writexl::write_xlsx(Jan_Feb_Data, 'Jan_Feb_Data.xlsx')
