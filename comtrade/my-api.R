library(httr)
library(jsonlite)
library(lubridate)
options(stringsAsFactors = FALSE)
url = "https://comtrade.un.org/api/get/plus?max=5000&type=C&freq=A&px=HS&ps=2012&r=51&p=0&rg=1&cc=AG6"
data <- httr::GET(url)
data$status_code
head(data$content)
yeet <- fromJSON(rawToChar(data$content))
names(yeet)
data <-yeet$dataset

data <- as.data.frame()

get_data <- function(){
  df = NULL
  year = c(200301,200302)
  for(i in year){
    url = paste0("https://comtrade.un.org/api/get/plus?max=5000&type=C&freq=M&px=HS&ps=",
                 i,"&r=51&p=0&rg=1&cc=ALL")
    temp <- httr::GET(url)
    temp <- fromJSON(rawToChar(temp$content))
    temp <- temp$dataset
    df = rbind(df,temp)
  }
  return(df)
}
data <- get_data()
