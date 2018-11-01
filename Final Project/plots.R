library(stringr)
library(utf8)
library(ggplot2)

library(dplyr)
library(writexl)

df  <- readxl::read_xlsx("to_work.xlsx", sheet = "Armstat", col_names = TRUE) #Reading the file
colnames(df) <- c('Name','ID','Year','Period','Export_in_tonnas',
                  'Export','Import_in_tonnas','Import')

df[,2:8] <- apply(df[,2:8],2,as.numeric)

df$Export <- df$Export/1000
df$Import <- df$Import/1000
main  <- readxl::read_xlsx("main.xlsx" ,sheet = "Sheet1", col_names = FALSE) 
colnames(main) <-  c('Name','ID')
main$ID<-as.numeric(main$ID)

main[main$ID==9701 & !(is.na(main$ID)),'Name']  <- "Ձեռքով արված նկար"
main[main$ID==9702 & !(is.na(main$ID)),'Name']  <- "Փորագրանկարի, Էստամպի, վինատպության բնօրինակներ"
main[main$ID==9704 & !(is.na(main$ID)),'Name']  <- "Քանդակների, արձանիկների բնօրինակներ	"
main[main$ID==9703 & !(is.na(main$ID)),'Name']  <- "Նամականիշ կամ պետական տուրքի դրոշմանիշ"
main[main$ID==9706 & !(is.na(main$ID)),'Name']  <- "Հավաքածու կամ հավաքածուի առարկաներ"
main[main$ID==9705 & !(is.na(main$ID)),'Name']  <- "Հնաոճ իրեր 100ից ավելի տարիքով"
rep_main <- data.frame("Name" = rep(main$Name, 11),"ID"=rep(main$ID, 11))
rep_main$Year <- 0
year_1 <- data.frame("Year"=c(2007:2017), "i"=c(1:11))
for (i in c(0:10)){
  rep_main$Year[(i *1244): ((i+1)*1244)] <- year_1[i+1,"Year"]
}
titles <- main[is.na(main$ID),'Name']
counter <- 1
for (i in c(1:nrow(titles))){
  titles[i,'Group'] = counter
  counter = counter +1
}
test1 <- df %>%
  filter(Year != 2018) %>%
  group_by(ID,Year) %>%
  summarise(Export_in_tonnas = sum(Export_in_tonnas),
            Export = sum(Export),
            Import_in_tonnas = sum(Import_in_tonnas),
            Import = sum(Import))



test2 <- left_join(rep_main, test1, by = c("ID","Year"))
test2[4:7][is.na(test2[4:7])] <- 0
counter <- 0

for (i in c(1:nrow(test2))){
  if ((i-1)%%1244==0) {
    counter = 0
  }
  if (is.na(test2[i,'ID'])) {
    counter = counter +1
  }
  test2[i,'Group'] = counter
}

unique(test2$Group)

cat <- test2 %>%
  group_by(Group, Year) %>%
  summarise(Export_in_tonnas1 = sum(Export_in_tonnas),
            Export1 = sum(Export),
            Import_in_tonnas1 = sum(Import_in_tonnas),
            Import1 = sum(Import))

test3 <- left_join(titles, cat, by = "Group")
test3['Group'] <- NULL
test2$Name <- as.character(test2$Name)
final <- left_join(test2,test3, by = c("Name","Year"))

for (i in c(1:nrow(final))){
  if (is.na(final[i,'ID'])){
    final[i,4:7] <- final[i,9:12]
  }
}
final <- final[,-c(8:12)]
ids <- c(101)
temp  <- final[final$ID %in% c(ids),c("Import_in_tonnas","Year","ID")]
ggplot(temp) +
  geom_point(aes(x=temp[,2],y=temp[,1]), color="Red")+
  geom_line(aes(x=temp[,2],y=temp[,1])) + 
  scale_x_discrete(name ="Year",limits=c(2007:2017))
# ids <- c(202,203,204)
# a <- list()
# a <- ggplot()
# for (i in c(1:length(ids))){
#   
#   a[[i]] <- final[final$ID %in% c(ids[i]),c("Import_in_tonnas","Year","ID")]
#   
#   p <- p + geom_point(aes(x=a[[k]]$Year,y=a[[k]]$Import_in_tonnas),colour="Red")+
#     geom_line(aes(x=a[[k]]$Year,y=a[[k]]$Import_in_tonnas))
#   print(p)
#   Sys.sleep(1)
# }
# }
# p <- ggplot()
# 
# 
# p <-   p + geom_point(aes(x=a[[1]]$Year,y=a[[1]]$Import_in_tonnas),colour="Red")+
#     geom_line(aes(x=a[[1]]$Year,y=a[[1]]$Import_in_tonnas)) 
# p <- p +    geom_point(aes(x=a[[2]]$Year,y=a[[2]]$Import_in_tonnas),colour="Red")+
#     geom_line(aes(x=a[[2]]$Year,y=a[[2]]$Import_in_tonnas))

p
# a1 <- final[final$ID %in% c(202),c("Import_in_tonnas","Year","ID")]
# b1 <- final[final$ID %in% c(203),c("Import_in_tonnas","Year","ID")]
# test <- ggplot() + geom_point(aes(x=a1$Year,y=a1$Import_in_tonnas),colour="Red")+
#   geom_line(aes(x=a1$Year,y=a1$Import_in_tonnas))
# test <- test + geom_point(aes(x=b1$Year,y=b1$Import_in_tonnas),colour="Blue")+
#   geom_line(aes(x=b1$Year,y=b1$Import_in_tonnas))
# test

