library(dplyr)
library(rvest)   
library(stringr)
library(XML)

# For Sale
###########################
###########################
###########################
  # Apartments and Houses
get_table_sale <- function(page_num,type){  
  url <- paste0("http://www.yerkir-real.com/en/",type,"/for-sale/?pageestateID=",page_num,
                "&sortestateID=38&sortType=ASC&estateTypeID=7&estMode=b")
  html <- read_html(url)
  names <- html %>%html_nodes("h3") %>% html_text()
  prices <- html %>% html_nodes(".price") %>% html_text()
  a <- html %>% html_nodes(".itValue") %>% html_text()
  length(a)
  rooms <- c()
  floor <- c()
  area <- c()
  status <- c()
  if (length(a)!=80){
    print(page_num)
  }
  for (value in seq(1,length(a),4)){
    area <- c(area,a[value])
    rooms <- c(rooms,a[value+1])
    floor <- c(floor,a[value+2])
    status <- c(status,a[value+3])
  }
  return (data.frame(names,prices,rooms,floor,area,status))
}

  ## Apartments for Sale Complete
df1 <- get_table_sale(1,"apartments")
for (page_num in c(2:231)){
  print(page_num)
  df1 <- rbind(df1,get_table_sale(page_num,"apartments"))
}
df1$type <- "Appartments"
df1$status1<- "For Sale"
df1

  ## Houses for Sale
df2 <- get_table_sale(1,"house")
for(page_num in c(2:89)){
  print(page_num)
  df2 <- rbind(df2,get_table_sale(page_num,"house"))
}
df2$type <- "House"
df2$status1 <- "For Sale"
df2
df<- rbind(df1,df2)

  ## Commercial Property for Sale
get_tablee_sale <- function(page_num,type){  
  url <- paste0("http://www.yerkir-real.com/en/",type,"/for-sale/?pageestateID=",page_num,
                "&sortestateID=38&sortType=ASC&estateTypeID=7&estMode=b")
  html <- read_html(url)
  names <- html %>%html_nodes("h3") %>% html_text()
  prices <- html %>% html_nodes(".price") %>% html_text()
  a <- html %>% html_nodes(".itValue") %>% html_text()
  length(a)
  area <- c()
  floor <- c()
  status <- c()
  if (length(a)!=60){
    print(page_num)
  }
  for (value in seq(1,length(a),3)){
    area <- c(area,a[value])
    floor <- c(floor,a[value+1])
    status <- c(status,a[value+2])
  }
  return (data.frame(names,prices,floor,area,status))
}

df3 <- get_tablee_sale(1,"commercial-property")
for (page_num in c(2:92)){
  print(page_num)
  df3 <- rbind(df3,get_tablee_sale(page_num,"commercial-property"))
}
df3$type <- "Commercial Property"
df3$rooms <- "0"
df3$status1 <- "For Sale"
df3
df<- rbind(df,df3)
df$status1 <- "For Sale"
df
  ## Land for Sale

get_tableee_sale <- function(page_num,type){  
  url <- paste0("http://www.yerkir-real.com/en/",type,"/for-sale/?pageestateID=",page_num,
                "&sortestateID=38&sortType=ASC&estateTypeID=8&estMode=b")
  html <- read_html(url)
  names <- html %>%html_nodes("h3") %>% html_text()
  prices <- html %>% html_nodes(".price") %>% html_text()
  a <- html %>% html_nodes(".itValue") %>% html_text()
  length(a)
  area <- c()
  for (value in length(a)){
    area <- c(area,a[value])
  }
  return (data.frame(names,prices,area))
}
df4 <- get_tableee_sale(1,"land")
for (page_num in c(2:86)){
  print(page_num)
  df4 <- rbind(df4,get_tableee_sale(page_num,"land"))
}
df4$type <- "Land"
df4$rooms <- "0"
df4$status1 <- "For Sale"
df4
df<- rbind(df,df4)
df4$status1 <- "For Sale"
df

## For rent

get_table_rent <- function(page_num,type){  
  url <- paste0("http://www.yerkir-real.com/en/",type,"/for-rent/?pageestateID=",page_num,
                "&sortestateID=39&sortType=ASC&estateTypeID=7&estMode=r")
  html <- read_html(url)
  names <- html %>%html_nodes("h3") %>% html_text()
  prices <- html %>% html_nodes(".price") %>% html_text()
  a <- html %>% html_nodes(".itValue") %>% html_text()
  length(a)
  rooms <- c()
  floor <- c()
  area <- c()
  status <- c()
  if (length(a)!=80){
    print(page_num)
  }
  for (value in seq(1,length(a),4)){
    area <- c(area,a[value])
    rooms <- c(rooms,a[value+1])
    floor <- c(floor,a[value+2])
    status <- c(status,a[value+3])
  }
  return (data.frame(names,prices,rooms,floor,area,status))
}
## defining types of the buildings
df4 <- get_table_rent(1,"apartments")
df4
for (page_num in c(2:113)){
  df4 <- rbind(df4,get_table_rent(page_num,"apartments"))
}
df4$type <- "Appartments"
df4$status1 <- "For Rent"
df4$floor <- "-"
df4$status <- "-"
df4
df<- rbind(df,df4)

# For Rent
###########################
###########################
###########################
# Apartments and Houses

get_table_rent <- function(page_num,type){  
  url <- paste0("http://www.yerkir-real.com/en/",type,"/for-rent/?pageestateID=",page_num,
                "&sortestateID=39&sortType=ASC&estateTypeID=7&estMode=r")
  html <- read_html(url)
  names <- html %>%html_nodes("h3") %>% html_text()
  prices <- html %>% html_nodes(".price") %>% html_text()
  a <- html %>% html_nodes(".itValue") %>% html_text()
  length(a)
  rooms <- c()
  floor <- c()
  area <- c()
  status <- c()
  if (length(a)!=80){
    print(page_num)
  }
  for (value in seq(1,length(a),4)){
    area <- c(area,a[value])
    rooms <- c(rooms,a[value+1])
    floor <- c(floor,a[value+2])
    status <- c(status,a[value+3])
  }
  return (data.frame(names,prices,rooms,floor,area,status))
}

df5 <- get_table_rent(1,"house")
df5
for (page_num in c(2:19)){
  df5 <- rbind(df5,get_table_rent(page_num,"house"))
}
df5$type <- "House"
df5$status1 <- "For Rent"
df5
View(df5)

  ## Apartments for Rent Complete
df5 <- get_table_sale(1,"apartments")
for (page_num in c(2:113)){
  print(page_num)
  df5 <- rbind(df5,get_table_rent(page_num,"apartments"))
}
df5$type <- "Appartments"
df5$status1<- "For Sale"
df <- rbind(df5,df)
## Houses for Rent Complete
df6 <- get_table_rent(1,"house")
for (page_num in c(2:19)){
  print(page_num)
  df6 <- rbind(df6,get_table_rent(page_num,"house"))
}
df6$type <- "House"
df6$status1<- "For Rent"

df <- rbind(df6,df)



get_tablee_rent <- function(page_num,type){  
  url <- paste0("http://www.yerkir-real.com/en/",type,"/for-rent/?pageestateID=5&sortestateID=",page_num,
                "&sortType=ASC&estateTypeID=6&estMode=r")
  html <- read_html(url)
  names <- html %>%html_nodes("h3") %>% html_text()
  prices <- html %>% html_nodes(".price") %>% html_text()
  a <- html %>% html_nodes(".itValue") %>% html_text()
  length(a)
  area <- c()
  floor <- c()
  status <- c()
  if (length(a)!=60){
    print(page_num)
  }
  for (value in seq(1,length(a),3)){
    area <- c(area,a[value])
    floor <- c(floor,a[value+1])
    status <- c(status,a[value+2])
  }
  return (data.frame(names,prices,floor,area,status))
}


df7 <- get_tablee_rent(1,"commercial-property")
for (page_num in c(2:93)){
  print(page_num)
  df7 <- rbind(df7,get_tablee_rent(page_num,"commercial-property"))
}
df7$type <- "Commercial Property"
df7$rooms <- "0"
df7
df7$status1 <- "For Rent"
df<- rbind(df,df7)

get_tableee_rent <- function(page_num,type){  
  url <- paste0("http://www.yerkir-real.com/en/",type,"/for-sale/?pageestateID=",page_num,
                "&sortestateID=38&sortType=ASC&estateTypeID=8&estMode=b")
  html <- read_html(url)
  names <- html %>%html_nodes("h3") %>% html_text()
  prices <- html %>% html_nodes(".price") %>% html_text()
  a <- html %>% html_nodes(".itValue") %>% html_text()
  length(a)
  area <- c()
  for (value in length(a)){
    area <- c(area,a[value])
  }
  return (data.frame(names,prices,area))
}
df8 <- get_tableee_rent(1,"land")
for (page_num in c(2:86)){
  print(page_num)
  df8 <- rbind(df8,get_tableee_rent(page_num,"land"))
}
df8
df8$type <- "Land"
df8$rooms <- "0"
df8$status1 <- "For Rent"
df8$status1 <- "For Sale"
df8$floor <- "-"
df8$status <- "-"
df<- rbind(df,df8)

writexl::write_xlsx(df, "Yerkir-real.xlsx")


