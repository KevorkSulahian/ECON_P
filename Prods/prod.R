library(dplyr)
library(writexl)
library(stringr)
for_main = getwd()

main  <- readxl::read_xlsx("Trade_Map_-_List_of_exported_products_for_the_selected_product_(All_products).xlsx",
                           sheet = "Trade_Map_-_List_of_exported_pr", col_names = TRUE) #Reading the file
colnames(main)<- c('col1','col2') 
main <- main[-c(1:8),]
main$col1 <- substring(main$col1,2)
ID <- as.data.frame(cbind(main$col1,main$col2))
colnames(ID) <- c("Code",'Product label')
ID$Code <- as.character(ID$Code)

setwd("countries")
files=list.files(pattern=".xlsx")
setwd(for_main)

Year="2013"
for (i in 1:length(files)){
  path = files[i]
  country <-  readxl::read_xlsx(paste0("countries/",path),  sheet = "Trade_Map_-_List_of_products_ex", 
                                col_names = FALSE) 
  country_name <- as.character(country[1, 'X__1'])
  country_name <- substring(country_name,30)
  country <- country[complete.cases(country$X__2),]
  
  colnames(country) <- unlist(country[country$X__1 =='Code',])
  colnames(country) <- as.character(unlist(country[1,]))
  country = country[-1, ]
  country$Code <- substring(country$Code,2)
  cols <- colnames(country)
  name <- cols[grep(Year, cols)]
  if (length(name)!=0){
    test <- country[,c('Code',name)]
    
    colnames(test) <- c('Code',country_name )
    
  }
  else {
    test <- country[,'Code']
    test[country_name]=''
    
  }
  test$Code <- as.character(test$Code)
  ID <- left_join(ID,test,by='Code')
}

ID[is.na(ID)] <- 0

