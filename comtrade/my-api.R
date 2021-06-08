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
  year = c(2019)
  for(i in year){
    url = paste0("https://comtrade.un.org/api/get/plus?max=100000&type=C&freq=A&px=HS&ps=",
                 i,"&r=all&p=0&rg=2&cc=AG2")
    temp <- httr::GET(url)
    temp <- fromJSON(rawToChar(temp$content))
    temp <- temp$dataset
    df = rbind(df,temp)
  }
  return(df)
}
data <- get_data()

writexl::write_xlsx(data,'data.xlsx')

# write.csv(data,"data.csv")
