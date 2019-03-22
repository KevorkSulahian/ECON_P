library(dplyr)
library(rvest)   
library(stringr)
library(XML)

url <- "http://hr.am/"

html <- read_html(url)

#vacancy_pagination > a:last-child()

get_number_of_page <- function(html) {
  html %>%
    html_nodes(xpath = '//*[@id="vacancy_pagination"]') 
}
get_number_of_page(html)
