# General-purpose data wrangling
library(tidyverse)  

# Parsing of HTML/XML files  
library(rvest)    

# String manipulation
library(stringr)   

# Verbose regular expressions
library(rebus)     

# Eases DateTime manipulation
library(lubridate)

url <-'http://www.trustpilot.com/review/www.amazon.com'
# has 213 pages atm

get_last_page <-function(html) {
  
  pages_data <- html %>%
    html_nodes('.pagination-page') %>%
    html_text()
  
  # the second to the last one is the one
  
  pages_data[(length(pages_data))] %>%
    # take raw string
    unname() %>%
    as.numeric()
}

first_page <- read_html(url)
last_page_number <-get_last_page(first_page)


list_of_pages <- str_c(url, "?page=", 1:213)


get_reviews <- function(html) {
  html %>%
    html_nodes('.review-content__text') %>%
    html_text() %>%
    
    #trim additional white space
    str_trim() %>%
    #convert the list into a vector
    unlist()
}

get_reviewers_names <- function(html) {
  html %>%
    html_nodes('.consumer-information__name') %>%
    html_text() %>%
    str_trim() %>%
    unlist()
}

get_review_dates <- function(html){
  
  status <- html %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "review-date--tooltip-target", " " ))]') %>%
    # time is a tag
    html_attrs() %>%
    # get the second one
    map(2) %>%
    unlist()
  
  dates <- html %>% 
    html_nodes('time') %>% 
    html_attrs() %>% 
    map(1) %>% 
    # Parse the string into a datetime object with lubridate
    ymd_hms() %>%                 
    unlist()

  # Combine the status and the date information to filter one via the other
  return_dates <- tibble(status = status, dates = dates) %>%   
    # Only these are actual reviews
    filter(status == 'ndate') %>%              
    # Select and convert to vector
    pull(dates) %>%                            
    # Convert DateTimes to POSIX objects
    as.POSIXct(origin = '1970-01-01 00:00:00')
  
  length_reviews <- length(get_reviews(html))
  
  return_reviews <- if (length(return_dates)> length_reviews){
    return_dates[1:length_reviews]
  } else{
    return_dates
  }
  return_reviews
    
}

get_star_rating <- function(html) {
  pattern = "star-rating-" %R% capture(DIGIT)
  
  ratings<- html %>%
    html_nodes('star-rating') %>%
    html_attrs() %>%
    
    map(str_match, pattern = pattern)
    map(2) %>%
    unlist()
    
  ratings[2:length(ratings)]
}

get_data_table <- function(html, company_name) {
  
  reviews <- get_reviews(html) 
  
  reviewer_names <- get_reviewers_names(html)
  
  dates <- get_review_dates(html)
  
  ratings <- get_star_rating(html)

  combined_data <- tibble(reviewer = reviewer_names,
                          date = dates,
                          rating = ratings,
                          review = reviews)
  
  combined_data %>% 
    mutate(company = company_name) %>% 
    select(company, reviewer, date, rating, review)
}

get_data_from_url <- function(url, company_name){
  html <- read_html(url)
  get_data_table(html, company_name)
}

scrape_write_table <- function(url, company_name){
  
  
  # Generate the target URLs
  list_of_pages <- str_c(url, '?page=', 1:213)
  
  # Apply the extraction and bind the individual results back into one table, 
  # which is then written as a tsv file into the working directory
  list_of_pages %>% 
    # Apply to all URLs
    map(get_data_from_url, company_name) %>%  
    # Combine the tibbles into one tibble
    bind_rows() %>%                           
    # Write a tab-separated file
    write_tsv(str_c(company_name,'.tsv'))     
}

scrape_write_table(url, 'amazon')








