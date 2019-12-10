library(dplyr)
library(rvest)   
library(stringr)

# html <- read_html("data/Trade_Map_-_List_of_exported_products_for_the_selected_product_(All_products).html")
file_names=c((dir(path = "data")))

df <- html %>%
  html_node(xpath = '//*[@id="ctl00_PageContent_MyGridView1"]') %>%
  html_table()


for (i in file_names) {
  html <- read_html(paste0('data/',i))
  
  temp <- html %>%
    html_node(xpath = '//*[@id="ctl00_PageContent_MyGridView1"]') %>%
    html_table()
  
  print(i)
  
  if(i == 'Trade_Map_-_List_of_exported_products_for_the_selected_product_(All_products).html') {
    df = temp
    colnames(df)[3:7]= c(paste0("Total","_",c(2014:2018)))
  }
  else {
    try(
      {
      name = str_extract(i,'by_(.*)')
      name = str_replace(name, 'by_',"")
      name = str_replace(name, '.html',"")
      temp$`Product label` = NULL
      colnames(temp)[2:6]= c(paste0(name,"_",c(2014:2018)))
      
      df = merge(df,temp,by.x = 'Product code', by.y = "Code", all.x = T)
      }
    )
    
  }
}
