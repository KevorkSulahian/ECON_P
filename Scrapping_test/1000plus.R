# library(RSelenium)
# library(rvest)
# library(tidyverse)
# 
# rD <- RSelenium::rsDriver(browser="firefox", port=4556L, verbose=F)
# remDr <- rD[["client"]]
# 
# 
# remDr$navigate("https://www.fcc.gov/media/engineering/dtvmaps")
# zip <- "30308"
# 
# remDr$findElement(using = "id", value = "startpoint")$sendKeysToElement(list(zip))
# 
# remDr$findElements("id", "btnSub")[[1]]$clickElement()
# 
# Sys.sleep(5) # give the page time to fully load
# html <- remDr$getPageSource()[[1]]
# 
# 
# signals <- read_html(html) %>% # parse HTML
#   html_nodes("table.tbl_mapReception") %>% # extract table nodes with class = "tbl_mapReception"
#   .[3] %>% # keep the third of these tables
#   .[[1]] %>% # keep the first element of this list
#   html_table(fill=T) # have rvest turn it into a dataframe
# View(signals)
# 
# names(signals) <- c("rm", "callsign", "network", "ch_num", "band", "rm2") # rename columns
# 
# signals <- signals %>%
#   slice(2:n()) %>% # drop unnecessary first row
#   filter(callsign != "") %>% # drop blank rows
#   select(callsign:band) # drop unnecessary columns
# 
# head(signals)
# 


library(readxl)

ID1 <- read_xlsx("EIN-2018-2019.xlsx")
ID1 <- ID1[1]
ID1 <- ID1[7:544,]
names(ID1) <- "col"
ID1 <- unique(ID1$col)

ID2 <- read_xlsx("EIN-2019-2020.xlsx")
ID2 <- ID2[1]
ID2 <- ID2[7:412,]
names(ID2) <- "col"
ID2 <- unique(ID2$col)

ID <- c(ID1,ID2)
ID <- unique(ID)


remDr$close()
rD$server$stop()
rm(rD)
gc()

library(RSelenium)
library(rvest)
library(tidyverse)
# if (!require(RJSONIO)) install.packages("RSJONIO")
library(stringr)


ID

rD <- RSelenium::rsDriver(browser="internet explorer", port=1126L, verbose=F)
remDr <- rD[["client"]]


remDr$navigate("https://www.1000plus.am")
Sys.sleep(8)
