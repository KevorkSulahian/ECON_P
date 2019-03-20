library(dplyr)
library(rvest)   
library(stringr)
library(XML)

get_last_page <- function() {
  url <- "https://job.am/en/jobs?p=1"
  
  html<- read_html(url)
  
  get_last_page_number <- function(html) {
    html %>%
      html_nodes(xpath = '/html/body/div[3]/form/div/div[3]/div[16]/div/div/div/ul/li[5]/a') %>%
      html_attr(name = "href") %>%
      str_extract_all('[0-9]') %>%
      as.numeric()
  }
  
  page <- get_last_page_number(html)
  return(page)
}

last_page_number <- get_last_page()

get_data <- function(number) {
  url <- "https://job.am/en/jobs?p="
  url <- paste0(url,number)
  html <- read_html(url)
  
  ## start from here i guess
  
  get_links <- function(html) {
    html %>%
      html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "fontSize24", " " ))]') %>%
      html_attr(name = "href")
  }
  
  links <- get_links(html)
  links <- paste0("https://job.am", links)
  
  get_title <- function(html) {
    html %>%
      html_node(xpath = '/html/body/div[3]/div/div[2]/div[1]/div/div[1]/h1') %>%
      html_text() %>%
      unlist() %>%
      trimws()
  }
  #test
  # get_title(read_html(links[1]))
  
  get_company <- function(html) {
    html %>%
      html_node(xpath = '/html/body/div[3]/div/div[1]/div[2]/h3/a') %>%
      html_text() %>%
      unlist()
  }
  # test
  # get_company(read_html(links[1]))
  
  get_category <- function(html) {
    html %>%
      html_node(xpath = '/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/p/span') %>%
      html_text() %>%
      unlist()
  }
  # test
  # get_category(read_html(links[1]))
  
  get_salary <- function(html) {
    html %>%
      html_node(xpath = '/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[3]/p/span') %>%
      html_text()
  }
  # test
  get_salary(read_html(links[1]))
  
  get_description <- function(html) {
    temp <- html %>%
      html_node(xpath = '/html/body/div[3]/div/div[2]/div[2]/div/ul[2]') %>%
      html_text() %>%
      str_replace_all("\r\n", " ")
    if(is.na(temp)) {
      return(
        html %>%
          html_node(xpath = '/html/body/div[3]/div/div[2]/div[2]/div/ul') %>%
          html_text() %>%
          str_replace_all("\r\n", " ")
      )
    }
    return(temp)
  }
  #test
  # get_description(read_html(links[1]))
  
  get_location <- function(html) {
    html %>%
      html_node(xpath = '/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[2]/p/span') %>%
      html_text() %>%
      unlist() 
  }
  #test
  # get_location(read_html(links[1]))
  
  
  get_closed_date <- function(html) {
    html %>%
      html_node(xpath = '/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[4]/p/span') %>%
      html_text()
  }
  
  # get_closed_date(read_html(links[1]))
  
  
  data <- data.frame(title = character(), company = character(), category = character(),
                     salary = character(), description = character(),
                     location = character(), close_date = numeric())
  
  get_info <- function(links) {
    for (link in links) {
      link = read_html(link)
      
      temp <- data.frame(title = get_title(link), company = get_company(link), 
                         category =  get_category(link), salary = get_salary(link),
                         description =  get_description(link), 
                         location = get_location(link),
                         closed_date = get_closed_date(link))
      
      data <- rbind(data,temp)
      
    }
    
    return(data)
  }
  final <- get_info(links)
  return(final)
}

data <- data.frame(title = character(), company = character(), category = character(),
                   salary = numeric(), description = character(),
                   location = character(), close_date = numeric())

get_full_data <- function(number) {
  for (i in 1:number) {
    temp <- get_data(i)
    
    data <- rbind(data,temp)
  }
  return(data)
}

final_data <- get_full_data(last_page_number)

writexl::write_xlsx(final_data, "job.xlsx")
