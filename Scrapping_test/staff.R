library(dplyr)
library(rvest)   
library(stringr)
library(XML)


gets <- function(url_number) {
  url <- "https://www.staff.am/en/jobs?per_page=40&page="
  url <- paste0(url, url_number)
  html <- read_html(url)
  
  get_links <- function(html) {
    html %>%
      html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "width100", " " ))]') %>%
      html_attr(name = "href")
  }
  
  links <- get_links(html)
  
  links <- paste0("https://www.staff.am", links)
  
  
  # get fucntions continue 
  
  get_title <- function(html) {
    html %>%
      html_node(xpath = '//*[(@id = "job-post")]//h2') %>%
      html_text() %>%
      unlist()
  }
  #test
  # get_title(read_html(links[1]))
  
  get_company <- function(html) {
    html %>%
      html_node(xpath = '/html/body/div[2]/div[3]/div[3]/div[2]/div/a/h1') %>%
      html_text() %>%
      unlist()
  }
  # test
   # get_company(read_html(links[1]))
  
  get_employment_type <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="job-post"]/div[1]/div[3]/p[1]') %>%
      html_text() %>%
      unlist() %>%
      str_replace_all("Employment term: ","") %>%
      str_replace_all("\n"," ")
  }
  #test
  # get_employment_type(read_html(links[1]))
  
  get_category <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="job-post"]/div[1]/div[3]/p[2]') %>%
      html_text() %>%
      unlist() %>%
      str_replace_all("Category: ","") %>%
      str_replace_all("\n"," ")
  }
  # test
  # get_category(read_html(links[1]))
  
  
  get_location <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="job-post"]/div[1]/div[4]/p[2]') %>%
      html_text() %>%
      unlist() %>%
      str_replace_all("Location: ","") %>%
      str_replace_all("\n"," ")
  }
  #test
  # get_location(read_html(links[1]))
  
  
  get_closed_date <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="job-post"]/div[1]/div[2]/p') %>%
      html_text() %>%
      unlist() %>%
      str_replace_all("Deadline: ","") %>%
      str_replace_all("\n"," ")
  }
  # get_closed_date(read_html(links[1]))
  
  get_description <- function(html) {
    html %>%
      html_node(xpath = '//*[@id="job-post"]/div[2]') %>%
      html_text() 
  }
  # get_description(read_html(links[1]))
  
  data <- data.frame(title = character(), company = character(), employment_type = character(),
                     category = character(), location = character(), close_date = character(), description = character())
  
  get_info <- function(links) {
    for (link in links) {
      link = read_html(link)
      # link = links[6]
      # link = read_html(link)
      
      temp <- data.frame(title = get_title(link), company = get_company(link), 
                         emplyment_type = get_employment_type(link), category =  get_category(link),
                         location = get_location(link), closed_date = get_closed_date(link), description = get_description(link))
      
      data <- rbind(data,temp)
      
    }
    return(data) 
  }
  final <- get_info(links)  
  return(final)
}

multiple_gets <- function(number){
  data <- data.frame(title = character(), company = character(), employment_type = character(),
                     category = character(), location = character(), close_date = character())
  for(i in 1:number) {
    temp <- gets(number)
    data <- rbind(data,temp)
  }
  return(data)
}

scraped_data <- multiple_gets(5)
scraped_data2 <- scraped_data[!duplicated(scraped_data$description),]
scraped_data3 <- scraped_data %>% distinct(description, .keep_all = T)
writexl::write_xlsx(scraped_data, "staff.xlsx")
