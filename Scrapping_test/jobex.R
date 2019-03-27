library(dplyr)
library(rvest)   
library(stringr)
library(XML)

url <- 'https://jobex.am/'
html <- read_html(url)

get_links <- function(html) {
  html %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "views-field-title", " " ))]//a') %>%
    html_attr("href")
}

links<- get_links(html)
links <- paste0(url,links)
# link <- read_html(links[1])

get_title <- function(html) {
  html %>%
    html_node(xpath = '//*[@id="post-content"]/h1') %>%
    html_text() %>%
    unlist() %>%
    trimws()
}
#test
# get_title(link)

get_category <- function(html) {
  html %>%
    html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "field-type-taxonomy-term-reference", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "even", " " ))]') %>%
    html_text() %>%
    unlist() %>%
    trimws()
}
#test
# get_category(link)

get_company <- function(html) {
  html %>%
    html_node(xpath = '//*[(@id = "block-system-main")]//*[contains(concat( " ", @class, " " ), concat( " ", "field-content", " " ))]') %>%
    html_text() %>%
    unlist() %>%
    trimws()
}
#test
# get_company(link)

get_location <- function(html) {
  html %>%
    html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "field-name-field-location", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "even", " " ))]') %>%
    html_text() %>%
    unlist() %>%
    trimws()
}
#test
# get_location(link)

get_employment_type <- function(html) {
  html %>%
    html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "field-name-field-working-time", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "even", " " ))]') %>%
    html_text() %>%
    unlist() %>%
    trimws()
}
#test
# get_employment_type(link)

get_description <- function(html) {
  html %>%
    html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "field-name-field-qualifications", " " ))]//p') %>%
    html_text() %>%
    unlist() %>%
    trimws()
}
#test
# get_description

get_open_date <- function(html) {
  html %>%
    html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "field-name-field-opening-date", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "date-display-single", " " ))]') %>%
    html_text() %>%
    unlist() %>%
    trimws()
}
#test
# get_open_date(link)

get_close_date <- function(html) {
  html %>%
    html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "field-name-field-close-date", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "date-display-single", " " ))]') %>%
    html_text() %>%
    unlist() %>%
    trimws()
}
#test
get_close_date(link)

data <- data.frame(title = character(), category = character(), company = character(),
                   location = character(), employment_type = character(), description = character(),
                   open_date = character(), close_date = character())

get_info <- function(links) {
  for (link in links) {
    link = read_html(link)
    
    temp <- data.frame(title = get_title(link), category = get_category(link), company = get_company(link),
                       location = get_location(link), employment_type = get_employment_type(link),
                       description = get_description(link),
                       open_date = get_open_date(link), close_date = get_close_date(link))
    
    data <- rbind(data,temp)
    
  }
  
  return(data)
}

final <- get_info(links)
writexl::write_xlsx(final,'jobex.xlsx')
