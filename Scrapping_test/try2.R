library(dplyr)
library(rvest)   
library(stringr)
library(XML)

first_website <- function() {
  url <- "http://www.jobfinder.am/"
  
  html <- read_html(url)
  
  ## start from here i guess
  get_new_links <- function(html) {
    html %>%
      html_nodes(xpath = '//*[(@id = "ctl00_bdyPlaceHolde_grdJobs_JFTabContainer_JFJobs_grdResultView")]//a[(((count(preceding-sibling::*) + 1) = 5) and parent::*)]') %>%
      html_attr(name = "href")
  }
  
  new_links <- get_new_links(html)
  new_links <- paste0("http://www.jobfinder.am/", new_links)
  
  get_links<- function(html) {
    html %>%
      html_nodes(xpath = '//*[(@id = "ctl00_bdyPlaceHolde_grdJobs_JFTabContainer_JFJobs_grdResultView")]//a[(((count(preceding-sibling::*) + 1) = 4) and parent::*)]') %>%
      html_attr(name = "href")
  }
  
  links <- get_links(html)
  links <- paste0("http://www.jobfinder.am/", links)
  # links <- links[1:]
  # test for 1
  # link1 <- links[1]
  
  links <- c(new_links, links)
  
  get_title <- function(html) {
    html %>%
      html_node(xpath = '//*[(@id = "ctl00_bdyPlaceHolde_jfpanelViewJob_jfJobPreview_lblJobPostTitle")]') %>%
      html_text() %>%
      unlist()
  }
  #test
  # get_title(link)
  
  get_company <- function(html) {
    html %>%
      html_node(xpath = '//*[(@id = "ctl00_bdyPlaceHolde_jfpanelViewJob_jfJobPreview_lnkCompany")]') %>%
      html_text() %>%
      unlist()
  }
  # test
  # get_company(link)
  
  get_employment_type <- function(html) {
    html %>%
      html_node(xpath = '//*[(@id = "ctl00_bdyPlaceHolde_jfpanelViewJob_jfJobPreview_lblPositionType")]') %>%
      html_text() %>%
      unlist()
  }
  #test
  # get_employment_type(link)
  
  get_category <- function(html) {
    html %>%
      html_node(xpath = '//*[(@id = "ctl00_bdyPlaceHolde_jfpanelViewJob_jfJobPreview_lblCategory")]') %>%
      html_text() %>%
      unlist()
  }
  # test
  #get_category(link)
  
  
  get_experience <- function(html) {
    number <- html %>%
      html_node(xpath = '//*[(@id = "ctl00_bdyPlaceHolde_jfpanelViewJob_jfJobPreview_lblExperience")]') %>%
      html_text() %>%
      str_extract_all('\\d', simplify = T)
    
    if (length(number) == 0) {
      return(0)
    } else {
      return(as.numeric(number))
    }
  }
  #test
  # aa <- get_experience(link)
  
  get_education <- function(html) {
    html %>%
      html_node(xpath = '//*[(@id = "ctl00_bdyPlaceHolde_jfpanelViewJob_jfJobPreview_lblEducation")]') %>%
      html_text() %>%
      unlist()
  }
  # test
  # get_education(link)
  
  get_salary <- function(html) {
    html %>%
      html_nodes(xpath = '//*[(@id = "ctl00_bdyPlaceHolde_jfpanelViewJob_jfJobPreview_lblSalary")]') %>%
      html_text() %>%
      unlist()
  }
  
  
  get_location <- function(html) {
    html %>%
      html_node(xpath = '//*[(@id = "ctl00_bdyPlaceHolde_jfpanelViewJob_jfJobPreview_lblLocation")]') %>%
      html_text() %>%
      unlist()
  }
  #test
  #get_location(link)
  
  get_open_date <- function(html) {
    html %>%
      html_node(xpath = '//*[(@id = "ctl00_bdyPlaceHolde_jfpanelViewJob_jfJobPreview_lblDate")]') %>%
      html_text() %>%
      substr(0,14)
  }
  #test
  # get_open_date(link)
  
  get_closed_date <- function(html) {
    html %>%
      html_node(xpath = '//*[(@id = "ctl00_bdyPlaceHolde_jfpanelViewJob_jfJobPreview_lblDate")]') %>%
      html_text() %>%
      substr(19, 32)
  }
  # get_closed_date(link)
  
  
  data <- data.frame(title = character(), company = character(), employment_type = character(), category = character(),
                     experience = integer(), education = character(), salary = character(),
                     location = character(), open_date = numeric(), close_date = numeric())
  
  get_info <- function(links) {
    for (link in links) {
      link = read_html(link)
      # link = links[6]
      # link = read_html(link)
      
      temp <- data.frame(title = get_title(link), company = get_company(link), 
                         emplyment_type = get_employment_type(link), category =  get_category(link),
                         experience = get_experience(link), education =  get_education(link), 
                         salary = get_salary(link),
                         location = get_location(link), open_date =  get_open_date(link),
                         closed_date = get_closed_date(link))
      
      data <- rbind(data,temp)
      
    }
    
    return(data)
  }
  final <- get_info(links)
  return(final)
}

data <- first_website()

writexl::write_xlsx(data, "jobfinder.xlsx")

