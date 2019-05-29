library(dplyr)
library(rvest)   
library(stringr)
library(XML)

url <- "https://www.estate.am/"
html <- read_html(url)
html %>%
  html_table("styled") 
 