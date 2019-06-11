library(readxl)

df <- read_xlsx('data18.xlsx')
df <- df[!nchar(df$`Code GCR`) > 5,]
df <- df[,colSums(is.na(df))<nrow(df)]
df <- df[!nchar(df$`Code GCR`) < 4,]
# df <- df[!df$`Code GCR` %in% '']
df <- df[grep('^[0-9]', df$`Code GCR`),]

df <- df[,-c(1:5)]
#############
#############       ASK FOR HELP, TOO MANY NA'S DELETING TOO MANY COLUMNS
############# 
df <- df[sapply(df, function(x) !any(is.na(x)))] 

apply(df,2,mean)
apply(df,2,var)

pca.out = prcomp(df,scale = T)
pca.out 
names(pca.out)
biplot(pca.out,scale=0, cex=.7,
       xlim = c(-5,10), ylim = c(-5,5))

# kmeans

km.out =kmeans(df,10,nstart = 15)
km.out
plot(df,col=km.out$cluster, pch=1, lwd=2, cex=2)
points(x,col=which,pch=19)
points(x,col=c(4,3,2,1)[which],pch=19)



# hierchal learning

hc.complete= hclust(dist(df), method = 'complete')
plot(hc.complete)
hc.single= hclust(dist(df), method = 'single')
plot(hc.single)
hc.average= hclust(dist(df), method = 'average')
plot(hc.average)

hc.cut = cutree(hc.complete,10)

table(hc.cut, km.out$cluster)





