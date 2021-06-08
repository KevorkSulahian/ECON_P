library(dplyr)
library(rvest)   
library(stringr)
library(XML)


number = 1
url <- "https://nkrmil.am/news/page/"
url <- paste0(url,number)
html <- read_html(url)

get_links <- function(html) {
  html %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "info", " " ))]//h4') %>%
    html_text()
}
links <- get_links(html)

links <- paste0("https://www.myjob.am/", links)
