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
  
  if(i == 'Trade_Map_-_List_of_exported_products_for_the_selected_product_(All_products).html') {
    df = temp
  }
  else {
    df = merge(df,temp,by = 'Product code')
  }
}