library(dplyr)
library(rvest)   
library(stringr)


all_links <- function(html) {
  html %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "job_load_more", " " ))]') %>%
    html_attr(name = "href")
}

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
    html_node(xpath = '//*[@id="header_info_block"]/div/div[1]/div[1]/a/h1') %>%
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
#test
# get_closed_date(read_html(links[1]))

get_description <- function(html) {
  html %>%
    html_node(xpath = '//*[@id="job-post"]/div[2]') %>%
    html_text() 
}


get_links <- function(url_number) {
  links = c()
  print(class(url_number))
  for (i in 1:as.numeric(url_number)) {
    url <- "https://www.staff.am/en/jobs?page="
    url2 <- '&per-page=50'
    url3 <- paste0(url, i,url2)
    
    lines <- readLines("scrape_final.js")
    lines[1] <- paste0("var url ='", url3 ,"';")
    
    writeLines(lines, "scrape_final.js")
    
    system("phantomjs scrape_final.js")
    
    html <- read_html('1.html')
    
    temp_links <- all_links(html)
    
    temp_links <- paste0("https://www.staff.am", temp_links)
    links = c(links,temp_links)
  }
  return(links)
}


data <- data.frame(title = character(), company = character(), employment_type = character(),
                   category = character(), location = character(), close_date = character(), description = character())

get_info <- function(links) {
  for (link in links) {
    # print(link)
    
    link = tryCatch({
      link2 = read_html(link)
      
      temp <- data.frame(title = get_title(link2), company = get_company(link2), 
                         emplyment_type = get_employment_type(link2), category =  get_category(link2),
                         location = get_location(link2), closed_date = get_closed_date(link2), description = get_description(link2))
      
      
    }, 
    error= function(e){
      print(link)
    })
      
    # link = links[6]
    # link = read_html(link)
    
    
    data <- rbind(data,temp)
    
  }
  return(data) 
}

links = get_links(13)
final <- get_info(links)  
writexl::write_xlsx(final, "staff.xlsx")
