library(sjmisc)
library(readxl)
library(treemap)
library(ggplot2)
df <- read_excel("try.xlsx")
df$iratsum = as.numeric(df$iratsum)
df$iratsum
class(df$iratsum)

treemap(df,index=c('ID','name','artank'),vSize='iratsum')
labelAlign= list(c('left','center'),c('left','top'), c('right','bottom'))
png(width = 1336,height=768, file='uspop_treemap')


geom_treemap(mapping = aes('Sport','City','Discipline','Year'), data = df, stat = "identity",
             position = "identity", na.rm = FALSE, show.legend = NA,
             inherit.aes = TRUE, fixed = NULL, layout = "squarified",
             start = "bottomleft")

