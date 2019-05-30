library(dplyr)
library(rvest)   
library(stringr)
library(XML)


first_website <- function(url) {
  # url <- "https://ararat-realty.com/en/search/2/sale-apartment"
  
  html <- read_html(url)
  
  
  get_links<- function(html) {
    html %>%
      html_nodes(xpath = '  //*[@id="products"]/div/div/a') %>%
      html_attr(name = "href")
  }
  
  links <- get_links(html)
  
  get_names <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="content_right"]/div[2]') %>%
      html_text() %>%
      unlist() %>%
      str_trim()
  }
  #test
  # get_names(link)
  
  get_area<- function(html) {
    html %>%
      html_node(xpath = '//*[@id="info"]/ul/li[2]/span') %>%
      html_text() %>%
      unlist()
  }
  # test
  # get_area(link)
  
  get_rooms <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="info"]/ul/li[1]/span') %>%
      html_text() %>%
      unlist()
  }
  #test
  # get_rooms(link)
  
  get_price <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="info"]/div[1]/span/b') %>%
      html_text() %>%
      unlist()
  }
  # test
  # get_price(link)
  
  
  get_floor <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="info"]/ul/li[3]/span') %>%
      html_text() %>%
      unlist()
  }
  # test
  # get_floor(link)
  
  data <- data.frame(names = character(), area = character(), room = character(),
                     price = character(), floor = character())
  
  get_info <- function(links) {
    for (link in links) {
      link = read_html(link)
      # link = links[6]
      # link = read_html(link)
      
      temp <- data.frame(names = get_names(link), area = get_area(link),
                         room = get_rooms(link), price = get_price(link),
                         floor = get_floor(link))
      
      data <- rbind(data,temp)
      
    }
    
    return(data)
  }
  final <- get_info(links)
  return(final)
}
loop_it <- function(url) {
  
  html <- read_html(url)
  
  last_boi<- function(html) {
    html %>%
      html_nodes(xpath = '//*[@id="main"]/div[4]/div/div/a[18]') %>%
      html_text() %>%
      as.numeric()
  }
  data <- data.frame(names = character(), area = character(), room = character(),
                     price = character(), floor = character())
  num <- last_boi(html)
  for (i in 1:num) {
    temp_url <- paste0(url,i)
    temp <- first_website(temp_url)
    data <- rbind(data,temp)
  }
  return(data)
}



sale-apartment <- loop_it('https://ararat-realty.com/en/search/2/sale-apartment/?page=')
sale-house <- loop_it('https://ararat-realty.com/en/search/3/sale-house/?page=')
sale-commercial <- loop_it('https://ararat-realty.com/en/search/4/sale-commercial/?page=')
sale-land <- loop_it('https://ararat-realty.com/en/search/5/sale-land/?page=')
rent-apartment <- loop_it('https://ararat-realty.com/en/search/6/rent-apartment/?page=')
rent-commerical <- loop_it('https://ararat-realty.com/en/search/8/rent-commercial/?page=')
rent-house <- loop_it('https://ararat-realty.com/en/search/7/rent-house/?page=')


# writexl::write_xlsx(data, "jobfinder.xlsx")

