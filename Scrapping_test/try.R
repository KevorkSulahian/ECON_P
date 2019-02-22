library(dplyr)
library(rvest)   
library(stringr)
library(XML)


get_data <- function(number) {
  url <- "https://www.myjob.am/Default.aspx?pg="
  url <- paste0(url,number)
  html <- read_html(url)
  
  ## start from here i guess
  
  get_links <- function(html) {
    html %>%
      html_nodes(xpath = '//*[@id="MainContentPlaceHolder_jobPageContainer"]/a') %>%
      html_attr(name = "href")
  }
  
  links <- get_links(html)
  links <- paste0("https://www.myjob.am/", links)
  
  get_title <- function(html) {
    html %>%
      html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "fullJobTextLong", " " )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]') %>%
      html_text() %>%
      unlist()
  }
  #test
  # get_title(read_html(links[1]))
  
  get_company <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="MainContentPlaceHolder_jobContainer"]/div[1]') %>%
      html_text() %>%
      unlist()
  }
  # test
  # get_company(read_html(links[1]))
  
  get_category <- function(html) {
    html %>%
      html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "fullJobTextsShort", " " ))]//div[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>%
      html_text() %>%
      unlist()
  }
  # test
  # get_category(read_html(links[1]))
  
  
  get_description <- function(html) {
    html %>%
      html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "fullJobTextLong", " " )) and (((count(preceding-sibling::*) + 1) = 9) and parent::*)]') %>%
      html_text() 
  }
  #test
  # get_description(read_html(links[1]))
  
  get_location <- function(html) {
    html %>%
      html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "fullJobTextShortMiddle", " " ))]') %>%
      html_text() %>%
      unlist()
  }
  #test
  # get_location(read_html(links[1]))
  
  
  get_closed_date <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="MainContentPlaceHolder_jobContainer"]/div[5]/div[3]') %>%
      html_text()
  }
  
  # get_closed_date(read_html(links[1]))
  
  
  data <- data.frame(title = character(), company = character(), category = character(),
                     description = character(),
                     location = character(), close_date = numeric())
  
  get_info <- function(links) {
    for (link in links) {
      link = read_html(link)
      
      temp <- data.frame(title = get_title(link), company = get_company(link), 
                         category =  get_category(link),
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

url <- "https://www.myjob.am/"
html <- read_html(url)

get_number <- function(html) {
  html %>%
    html_node(xpath = '//*[@id="MainContentPlaceHolder_resultsFound"]') %>%
    html_text() %>%
    str_extract_all('[0-9]')
}

number <- get_number(html)
number <- as.numeric(number[[1]])
number <- paste0(number, collapse = "")
number <- as.numeric(number)
number <- number/10
number <- ceiling(number)

data <- data.frame(title = character(), company = character(), category = character(),
                   description = character(),
                   location = character(), close_date = numeric())

get_full_data <- function(number) {
  for (i in 1:number) {
    temp <- get_data(i)
    
    data <- rbind(data,temp)
  }
  return(data)
}


final_data <- get_full_data(number)

writexl::write_xlsx(final_data, "myjob.xlsx")
