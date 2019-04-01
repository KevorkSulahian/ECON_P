library(dplyr)
library(rvest)   
library(stringr)
library(XML)
options(timeout = 4000000)

get_data <- function() {
  url <- "https://rezume.am/"
  
  html <- read_html(url)
  
  get_number_of_page <- function(html) {
    html %>%
      html_nodes(xpath = '//*[@id="loyal"]/div[2]/nav/ul/li[6]/a') %>%
      html_attr(name = "href") %>%
      str_extract_all("\\d") 
  }
  a <- get_number_of_page(html)
  a <- as.numeric(a[[1]])
  number_of_links <- NULL
  for (i in a) {
    number_of_links <- paste0(number_of_links,i)
  }
  number_of_links <- as.numeric(number_of_links)
  number_of_pages <- ceiling(number_of_links / 30)
  
  links <- NULL
  
  get_links <- function(html) {
    html %>%
      html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "addhref", " " ))]') %>%
      html_attr(name = "href")
  }
  
  
  for (i in c(0:number_of_pages)) {
    i = i*30
    link <- paste0("https://rezume.am/", i)
    
    html <- read_html(link)
    
    temp_links <- get_links(html)
    links <- c(links, temp_links)
  }
  # link <- read_html(links[1])
  ## big boi functions here
  get_title <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="loyal"]/div[2]/div/div[1]/h4') %>%
      html_text() %>%
      unlist()
  }
  #test
  # get_title(link)
  
  get_company <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="loyal"]/div[1]/div[2]/div[2]/div[1]/h4') %>%
      html_text() %>%
      unlist()
  }
  # test
  # get_company(link)
  
  get_employment_type <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="loyal"]/div[1]/div[2]/div[1]/div[1]') %>%
      html_text() %>%
      unlist() %>%
      str_replace_all("\n","") %>%
      trimws()
  }
  #test
  # get_employment_type(link)
  
  # use title instead of category
  
  
  get_experience <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="loyal"]/div[1]/div[2]/div[1]/div[2]/span[3]') %>%
      html_text()
  }
  #test
  # get_experience(link)
  
  get_education <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="loyal"]/div[1]/div[2]/div[1]/div[2]/span[2]') %>%
      html_text() %>%
      unlist()
  }
  # test
  # get_education(link)
  
  get_salary <- function(html) {
    html %>%
      html_nodes(xpath = '//*[@id="loyal"]/div[1]/div[2]/div[1]/div[2]/strong') %>%
      html_text() %>%
      unlist()
  }
  #test
  # get_salary(link)
  
  get_closed_date <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="loyal"]/div[1]/div[2]/div[1]/div[2]/text()[2]') %>%
      html_text()
  }
  # get_closed_date(link)
  
  
  data <- data.frame(title = character(), company = character(), employment_type = character(), category = character(),
                     experience = integer(), education = character(),
                     salary = character(), close_date = numeric())
  
  get_info <- function(links) {
    for (linko in links) {
      download.file(linko, destfile = "scrapedpage.html", quiet=TRUE)
      link = read_html("scrapedpage.html")
      # link = links[6]
      # link = read_html(link)
      
      temp <- data.frame(title = get_title(link), company = get_company(link), 
                         emplyment_type = get_employment_type(link), category =  get_title(link),
                         experience = get_experience(link), education =  get_education(link), 
                         salary = get_salary(link),
                         closed_date = get_closed_date(link))
      
      data <- rbind(data,temp)
      
    }
    
    return(data)
  }
  final <- get_info(links)
  return(final)
}
data <- get_data()
writexl::write_xlsx(data,"rezume.xlsx")
