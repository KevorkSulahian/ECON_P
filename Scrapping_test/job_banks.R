library(dplyr)
library(rvest)   
library(stringr)
library(XML)


get_data <- function() {
  url <- "https://job.banks.am/am"
  
  html <- read_html(url)
  
  get_link <- function(html) {
    html %>%
      html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "relative", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "col-sm-8", " " ))]//a') %>%
      html_attr(name = "href") 
  }
  links <- get_link(html)
  links <- paste0("https://job.banks.am",links)
  
  # link <- read_html(links[1])
  
  ## big boi functions here
  get_title <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="home"]/div[2]/div/p') %>%
      html_text() %>%
      unlist()
  }
  #test
  # get_title(link)
  # 
  get_company <- function(html) {
    temp_link <- html %>%
      html_node(xpath = '//*[@id="home"]/div[1]/div[4]/a') %>%
      html_attr(name = "href") 
    temp_link <- paste0("https://job.banks.am", temp_link)
    html_temp <- read_html(temp_link)
    title <- html_temp %>%
      html_node(xpath ='//*[@id="home"]/p[1]') %>%
      html_text()
    return(title)
  }
  # test
  # get_company(link)
  
  get_employment_type <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="home"]/div[20]/div/p') %>%
      html_text() %>%
      unlist()
  }
  #test
  # get_employment_type(link)
  
  get_category <- function(link) {
    temp <- html %>%
      html_node(xpath = '//*[@id="home"]/div[4]/div') %>%
      html_text() 
    if(is.na(temp)) return("")
  }
  #test
  # get_category(link)
  
  get_experience <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="home"]/div[10]/div/p') %>%
      html_text()
  }
  #test
  # get_experience(link)
  
  get_education <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="home"]/div[8]/div/p') %>%
      html_text() %>%
      unlist()
  }
  # test
  # get_education(link)
  
  get_salary <- function(html) {
    html %>%
      html_nodes(xpath = '//*[@id="home"]/div[14]/div/p') %>%
      html_text() %>%
      unlist()
  }
  #test
  # get_salary(link)
  get_location <- function(link) {
    html %>%
      html_nodes(xpath = '//*[@id="home"]/div[6]/div/p') %>%
      html_text() %>%
      unlist()
  }
  #test
  # get_location(link)
  
  get_closed_date <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="home"]/div[28]/div/p[2]') %>%
      html_text()
  }
  # get_closed_date(link)
  
  
  data <- data.frame(title = character(), company = character(), employment_type = character(), category = character(),
                     experience = integer(), education = character(),
                     salary = character(), close_date = numeric())
  
  get_info <- function(links) {
    for (linko in links) {
      # download.file(linko, destfile = "scrapedpage.html", quiet=TRUE)
      link = read_html(linko)
      # link = links[6]
      # link = read_html(link)
      
      temp <- data.frame(title = get_title(link), company = get_company(link), 
                         emplyment_type = get_employment_type(link), category =  get_category(link),
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
writexl::write_xlsx("job_banks.xlsx")