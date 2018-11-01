library(stringr)
library(utf8)
library(ggplot2)

library(dplyr)
library(writexl)
df  <- readxl::read_xlsx("to_work.xlsx", sheet = "Armstat", col_names = TRUE) #Reading the file
colnames(df) <- c('Name','ID','Year','Period','Export_in_tonnas',
                  'Export','Import_in_tonnas','Import')


# df$Period <- NULL
df[,2:8] <- apply(df[,2:8],2,as.numeric)

df$Export <- df$Export/1000
df$Import <- df$Import/1000
########Reading the main file
main  <- readxl::read_xlsx("main.xlsx" ,sheet = "Sheet1", col_names = FALSE) 
colnames(main) <-  c('Name','ID')
main$ID<-as.numeric(main$ID)

main[main$ID==9701 & !(is.na(main$ID)),'Name']  <- "Ձեռքով արված նկար"
main[main$ID==9702 & !(is.na(main$ID)),'Name']  <- "Փորագրանկարի, Էստամպի, վինատպության բնօրինակներ"
main[main$ID==9704 & !(is.na(main$ID)),'Name']  <- "Քանդակների, արձանիկների բնօրինակներ	"
main[main$ID==9703 & !(is.na(main$ID)),'Name']  <- "Նամականիշ կամ պետական տուրքի դրոշմանիշ"
main[main$ID==9706 & !(is.na(main$ID)),'Name']  <- "Հավաքածու կամ հավաքածուի առարկաներ"
main[main$ID==9705 & !(is.na(main$ID)),'Name']  <- "Հնաոճ իրեր 100ից ավելի տարիքով"
#Seperating titles and assigning groups to it
titles <- main[is.na(main$ID),'Name']
counter <- 1
for (i in c(1:nrow(titles))){
  titles[i,'Group'] = counter
  counter = counter +1
}
##############
counter <-1
for (i in c(1:nrow(main))){
  if (is.na(main[i,'ID'])){
    main[i,'ID'] <- counter
    counter <- counter + 1
  }
  
}
rep <- data.frame("Name" = rep(main$Name, 12),"ID"=rep(main$ID, 12))
rep1 <- rep %>% group_by(ID) %>% mutate(Period = row_number())
#########

mini <- df[df$Year==2017,]
mini$Name <- NULL
test1 <- left_join(rep1,mini,by=c("ID","Period"))
# test2 <- test1[order(test1$ID),]

test3 <- test1[test1$Period==1,]
test3$Year <- NULL

year_agg <- function(df, Year){
  df_Year <- df[df$Year == Year,]
  agg_df_Year <- df_Year %>%
    group_by(ID) %>%
    summarise(Export_in_tonnas = sum(Export_in_tonnas),
              Export = sum(Export),
              Import_in_tonnas = sum(Import_in_tonnas),
              Import = sum(Import))
  polufinal_Year_1 <- left_join(main, agg_df_Year, by = "ID")
  polufinal_Year_1['Year'] <- Year
  counter <- 0
  for (i in c(1:nrow(polufinal_Year_1))){
    polufinal_Year_1[i,'Group'] = counter
    
    if (is.na(polufinal_Year_1[i,'ID'])){
      counter = counter +1
    }
  }
  polufinal_Year_1[is.na(polufinal_Year_1$ID),'Group']<- NA
  polufinal_Year_1[3:6][is.na(polufinal_Year_1[3:6])] <- 0
  
  
  agg_total_Year <-polufinal_Year_1 %>%
    group_by(Group)  %>%
    summarise(Total_Export_in_tonnas = sum(Export_in_tonnas),
              Total_Export = sum(Export),
              Total_Import_in_tonnas = sum(Import_in_tonnas),
              Total_Import = sum(Import))
  agg_total_Year <- agg_total_Year[complete.cases(agg_total_Year), ]
  
  
  polufinal_Year_2 <- left_join(titles, agg_total_Year, by = "Group")
  polufinal_Year_2['Group'] <- NULL
  final_Year <- left_join(polufinal_Year_1,polufinal_Year_2, by = "Name")
  
  for (i in c(1:nrow(final_Year))){
    if (is.na(final_Year[i,'ID'])){
      final_Year[i,3:6] <- final_Year[i,9:12]
    }
  }
  final_Year <- final_Year[,-c(8:12)]
  
  
  return (final_Year)
}



# sum(is.na(test2$ID))

p1 <- mini %>%
  group_by(ID,Name) %>%
  summarise( a = rollapply(Export_in_tonnas, 2, sum)[1],
             b = rollapply(Export_in_tonnas, 3, sum)[1],
             c = rollapply(Export_in_tonnas, 4, sum)[1],
             d = rollapply(Export_in_tonnas, 5, sum)[1],
             e = rollapply(Export_in_tonnas, 6, sum)[1],
             f = rollapply(Export_in_tonnas, 7, sum)[1],
             g = rollapply(Export_in_tonnas, 8, sum)[1],
             h = rollapply(Export_in_tonnas,9, sum)[1]
             )
                       # Export = sum(Export),
                       # Import_in_tonnas = sum(Import_in_tonnas),
                       # Import = sum(Import))
rollapply(miniExport_in_tonnas, 2, sum)
x <- c(1, 2, 3, 10, 20, 30)
library(zoo)
rollapply(x, 2, sum)
##






