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

month_per <- data.frame(Month = c("Հունվար","Փետրվար","Մարտ","Ապրիլ","Մայիս","Հունիս","Հուլիս"
                               ,"Օգոստոս","Սեպտեմբեր",
                               "Հոկտեմբեր","Նոյեմբեր","Դեկտեմբեր"),
           Period = rep(1:12));

per <- month_per[month_per$Month %in% c("Ապրիլ"),"Period"]

# agg2 <- df[df$Period %in% per,]
# fin_m1 <- get_columns(agg2,input$Expimp)
# per <- month_per[month_per$Month==input,"Period"]
agg1 <- df[df$Period <= per,]
agg1$Period<-NULL

year_agg <- function(df, Year){

  df_Year <- df[df$Year == Year,]
  agg_df_Year <- df_Year %>%
    group_by(ID) %>%
    summarise(Export_in_tonnas = sum(Export_in_tonnas),
              Export = sum(Export),
              Import_in_tonnas = sum(Import_in_tonnas),
              Import = sum(Import))
  polufinal_Year_1 <- left_join(main, agg_df_Year, by = c("ID"))
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
  year<- final_Year$Year[1]
  final_Year$Year <- NULL
  colnames(final_Year) <- c("Name","ID",paste0(colnames(final_Year[,c(3:6)]), ".",year))
  
  return (final_Year)
}

years <- c(2008, 2011)
fin_m <- main
for (year in years){
  temp <- year_agg(agg1, year)
  fin_m <- inner_join(fin_m, temp, by = c("Name","ID"))
}

temp
# 
# text = "202-210"
# if  (grepl(',', text)){
#   code1 <- unlist(strsplit(text,','))
# } else if (grepl('-',text)){
#   code1 <- unlist(strsplit(text,'-'))
#   code1 <- seq(code1[1],code1[2])
# }
# 
# 
# test <- fin_m[fin_m$ID %in% code1,]

