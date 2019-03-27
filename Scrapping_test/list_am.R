library(dplyr)
library(rvest)   
library(stringr)
library(XML)
library(tm)

get_links <- function(number) {
  links <- NULL
  for (i in 1:number) {
    url <- paste0("https://www.list.am/category/29/", i)
    html<- read_html(url)
    temp <- html %>%
      html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "t", " " ))]//a') %>%
      html_attr(name ='href' )
    links <- c(links,temp)
  }
  return(links)
}

#big boi functions

# link <- read_html(links[1])
get_title <- function(html) {
  html %>%
    html_node(xpath = '//*[@id="pcontent"]/div/div[1]/div/h1') %>%
    html_text() %>%
    unlist() %>%
    trimws()
}
#test
# get_title(link)

get_company <- function(html) {
  html %>%
    html_node(xpath = '//*[@id="uinfo"]/div[1]/a') %>%
    html_text() %>%
    unlist()
}
# test
# get_company(link)

get_employment <- function(html) {
  html %>%
    html_node(xpath = '//*[@id="attr"]/div[1]/div[2]') %>%
    html_text() %>%
    unlist()
}
#test
# get_employment(link)

get_salary <- function(html) {
  html %>%
    html_node(xpath = '//*[@id="attr"]/div[3]/div[2]') %>%
    html_text()
}
# test
# get_salary(link)


get_location <- function(html) {
  html %>%
    html_node(xpath = '//*[@id="abar"]/div[2]/a') %>%
    html_text() %>%
    unlist()
}
#test
get_location(link)


get_open_date <- function(html) {
  html %>%
    html_node(xpath = '//*[@id="pcontent"]/div/div[6]/span[3]') %>%
    html_text() %>%
    removeWords('Renewed: ')
}
#test
# get_open_date(link)

get_description <- function(html) {
  html %>%
    html_node(xpath = '//*[@id="pcontent"]/div/div[5]') %>%
    html_text() %>%
    unlist()
}
#test
# get_description(link)

data <- data.frame(title = character(), company = character(), employment_type = character(),
                   salary = character(), location = character(), open_date = numeric(), description = character())

get_info <- function(links) {
  for (link in links) {
    link = read_html(link)
    # link = links[6]
    # link = read_html(link)
    
    temp <- data.frame(title = get_title(link), company = get_company(link), 
                       emplyment_type = get_employment(link),
                       salary = get_salary(link),
                       location = get_location(link), open_date =  get_open_date(link),
                       description =get_description(link))
    
    data <- rbind(data,temp)
    
  }
  
  return(data)
}



links<- get_links(1)
links<- unique(links)
links<- paste0('https://www.list.am/en',links)

final <- get_info(links)
