library(dplyr)
library(rvest)   
library(stringr)
library(XML)

# For Sale
###########################
###########################
###########################
# Apartments and Houses
get_table_sale <- function(page_num){  
  url <- paste0("https://myrealty.am/en/bnakaranneri-vacharq/11294?page=",page_num)
  html <- read_html(url)
  names <- html %>%html_nodes(".item-address") %>% html_text()
  prices <- html %>% html_nodes(".item-price") %>% html_text()
  area <- html %>% html_nodes(".item-sqm") %>% html_text()
  rooms <- html %>% html_nodes(".item-rooms") %>% html_text()
  floor <- html %>% html_nodes(".item-floor ") %>% html_text()
  return (data.frame(names,prices,rooms,floor,area))
}


## Apartments for Sale Complete
df1 <- get_table_sale(1)
for (page_num in c(2:218)){
  print(page_num)
  df1 <- rbind(df1,get_table_sale(page_num))
}
df1$type <- "Appartments"
df1$status1<- "For Sale"
df1
View(df1)

## Houses for Sale
get_tablee_sale <- function(page_num){  
  url <- paste0("https://myrealty.am/en/houses-for-sale/47183?page=",page_num)
  html <- read_html(url)
  names <- html %>%html_nodes(".item-address") %>% html_text()
  prices <- html %>% html_nodes(".item-price") %>% html_text()
  area <- html %>% html_nodes(".item-sqm") %>% html_text()
  rooms <- html %>% html_nodes(".item-rooms") %>% html_text()
  floor <- html %>% html_nodes(".item-floor ") %>% html_text()
  return (data.frame(names,prices,rooms,floor,area))
}
df2 <- get_tablee_sale(1)
for(page_num in c(2:60)){
  print(page_num)
  df2 <- rbind(df2,get_tablee_sale(page_num))
}
df2$type <- "House"
df2$status1 <- "For Sale"
df2
df<- rbind(df1,df2)

## Commercial Property for Sale
get_tableee_sale <- function(page_num,type){  
  url <- paste0("https://myrealty.am/en/commercial-properties-for-sale/50511?page=",page_num)
  html <- read_html(url)
  names <- html %>%html_nodes(".item-address") %>% html_text()
  prices <- html %>% html_nodes(".item-price") %>% html_text()
  area <- html %>% html_nodes(".item-sqm") %>% html_text()
  floor <- html %>% html_nodes(".item-floor ") %>% html_text()
  return (data.frame(names,prices,floor,area))
}

df3 <- get_tableee_sale(1)
for (page_num in c(2:24)){
  print(page_num)
  df3 <- rbind(df3,get_tableee_sale(page_num))
}
df3$type <- "Commercial Property"
df3$rooms <- "0"
df3$status1 <- "For Sale"
df3
df<- rbind(df,df3)
df
## Land for Sale

get_tableeee_sale <- function(page_num){  
  url <- paste0("https://myrealty.am/en/land-for-sale-armenia/47185?page=",page_num)
  html <- read_html(url)
  names <- html %>%html_nodes(".item-address") %>% html_text()
  prices <- html %>% html_nodes(".item-price") %>% html_text()
  area <- html %>% html_nodes(".item-sqm") %>% html_text()
  return (data.frame(names,prices,area))
}
df4 <- get_tableeee_sale(1)
for (page_num in c(2:18)){
  print(page_num)
  df4 <- rbind(df4,get_tableeee_sale(page_num))
}
df4$type <- "Land"
df4$rooms <- "0"
df4$status1 <- "For Sale"
df4$floor <- "-"
df4
df<- rbind(df,df4)
df

## For rent



# For Rent
###########################
###########################
  
get_table_rent <- function(page_num){  
  url <- paste0("https://myrealty.am/en/apartments-for-rent/400365?page=",page_num)
  html <- read_html(url)
  names <- html %>%html_nodes(".item-address") %>% html_text()
  prices <- html %>% html_nodes(".item-price") %>% html_text()
  area <- html %>% html_nodes(".item-sqm") %>% html_text()
  rooms <- html %>% html_nodes(".item-rooms") %>% html_text()
  floor <- html %>% html_nodes(".item-floor ") %>% html_text()
  return (data.frame(names,prices,rooms,floor,area))
}

  ## Apartments for Rent Complete
df5 <- get_table_rent(1)
for (page_num in c(2:170)){
  print(page_num)
  df5 <- rbind(df5,get_table_rent(page_num))
}
df5$type <- "Appartments"
df5$status1<- "For Rent"
df5
df<- rbind(df, df5)

##### Houses for Rent

get_tablee_rent <- function(page_num){  
  url <- paste0("https://myrealty.am/en/houses-for-rent/537835?page=",page_num)
  html <- read_html(url)
  names <- html %>%html_nodes(".item-address") %>% html_text()
  prices <- html %>% html_nodes(".item-price") %>% html_text()
  area <- html %>% html_nodes(".item-sqm") %>% html_text()
  rooms <- html %>% html_nodes(".item-rooms") %>% html_text()
  floor <- html %>% html_nodes(".item-floor ") %>% html_text()
  return (data.frame(names,prices,rooms,floor,area))

}

df6 <- get_tablee_rent(1)
df6
for (page_num in c(2:21)){
  print(page_num)
  df6 <- rbind(df6,get_tablee_rent(page_num))
}
df6$type <- "House"
df6$status1 <- "For Rent"
df6
View(df6)
df<- rbind(df,df6)


## Commercial for Rent
get_tableee_rent <- function(page_num,type){  
  url <- paste0("https://myrealty.am/en/commercial-properties-for-rent/375391?page=",page_num)
  html <- read_html(url)
  names <- html %>%html_nodes(".item-address") %>% html_text()
  prices <- html %>% html_nodes(".item-price") %>% html_text()
  area <- html %>% html_nodes(".item-sqm") %>% html_text()
  floor <- html %>% html_nodes(".item-floor ") %>% html_text()
  return (data.frame(names,prices,floor,area))
}

df7 <- get_tableee_rent(1)
for (page_num in c(2:24)){
  print(page_num)
  df7 <- rbind(df7,get_tableee_sale(page_num))
}
df7$type <- "Commercial Property"
df7$rooms <- "0"
df7$status1 <- "For Sale"
df7
df<- rbind(df,df7)
df

writexl::write_xlsx(df, "myrealty.xlsx")
