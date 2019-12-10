  library(dplyr)
  library(rvest)   
  library(stringr)
  library(XML)
  
  get_data <- function(number) {
    
    
    ## start from here i guess
    for(i in 1:number) {
      url <- "https://job.am/en/jobs?p="
      url <- paste0(url,i)
      html <- read_html(url)
      
      links <- c()
      
      get_links <- function(html) {
        temp <- html %>%
          html_nodes(xpath = '/html/body/div[2]/form/div[2]/div[2]/div/div[2]/div/div[1]/a') %>%
          html_attr(name = "href")
        
      }
      
      links <- c(links, get_links(html))
      links <- paste0("https://job.am", links)
    }
    
    
    html1 = read_html(links[1])
    get_title <- function(html) {
      html %>%
        html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "wordBreak", " " )) and contains(concat( " ", @class, " " ), concat( " ", "text-dark", " " ))]') %>%
        html_text() %>%
        unlist() %>%
        trimws()
    }
    #test
    # get_title(html1)
    
    get_company <- function(html) {
      html %>%
        html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "titelSize", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "color-dark", " " ))]') %>%
        html_text() %>%
        unlist()
    }
    # test
    # get_company(html1)
    
    get_category <- function(html) {
      html %>%
        html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "font", " " ))]') %>%
        html_text() %>%
        unlist()
    }
    # test
    # get_category(html1)
    
    get_description <- function(html) {
      html %>%
        html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "job-descr", " " ))]') %>%
        html_text()
    }
    # test
    # get_description(html1)
    
    get_location <- function(html) {
      html %>%
        html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "col-md-6", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "text-muted", " " ))]') %>%
        html_text() %>%
        unlist() 
    }
    #test
    # get_location(html1)
    
    
    get_closed_date <- function(html) {
      html %>%
        html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "col-md-6", " " )) and (((count(preceding-sibling::*) + 1) = 4) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "text-muted", " " ))]') %>%
        html_text()
    }
    
    # get_closed_date(html1)
    
    get_employment_type <- function(html) {
      html %>%
        html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "col-md-6", " " )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "text-muted", " " ))]') %>%
        html_text()
    }
    
    # get_employment_type(html1)
    
    data <- data.frame(title = character(), company = character(), category = character(),
                       description = character(),
                       location = character(), close_date = numeric(), employment = character())
    
    get_info <- function(links) {
      for (link in links) {
        link = read_html(link)
        
        temp <- data.frame(title = get_title(link), company = get_company(link), 
                           category =  get_category(link), #salary = get_salary(link),
                           description =  get_description(link),
                           location = get_location(link),
                           closed_date = get_closed_date(link),
                           employment = get_employment_type(link))
        
        data <- rbind(data,temp)
        
      }
      
      return(data)
    }
    final <- get_info(links)
    return(final)
  }
  
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
  
  final_data <- get_full_data(7)
  
  writexl::write_xlsx(final_data, "job.xlsx")
