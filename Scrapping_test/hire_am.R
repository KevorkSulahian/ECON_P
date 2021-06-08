# library(dplyr)
# library(rvest)
# library(stringr)
# library(XML)
# 
# get_links <- function(number) {
#   links <- NULL
#   for (i in 1:number) {
#     url <- paste0("http://www.hire.am/jobs?p=", i)
#     html<- read_html(url)
#     temp <- html %>%
#       html_nodes(xpath = '//*[@id="job-listings"]/div/span/a') %>%
#       html_attr(name ='href' )
#     links <- c(links,temp)
#   }
#   return(links)
# }
# 
# # a <- get_links(1)
# # link <- read_html(a[1])
# 
# get_title <- function(html) {
#   html %>%
#     html_node(xpath = '//*[@id="job-details"]/h2') %>%
#     html_text() %>%
#     unlist() %>%
#     trimws()
# }
# #test
# # get_title(link)
# 
# get_location <- function(html) {
#   html %>%
#     html_node(xpath = '//*[@id="job-details"]/p/strong[2]') %>%
#     html_text() %>%
#     unlist() %>%
#     trimws()
# }
# #test
# # get_location(link)
# 
# get_description <- function(html) {
#   html %>%
#     html_node(xpath = '//*[@id="job-description"]') %>%
#     html_text() %>%
#     unlist() %>%
#     trimws()
# }
# #test
# # get_description(link)
# 
# data <- data.frame(title = character(), location = character(), description = character())
# 
# get_info <- function(links) {
#   for (link in links) {
#     link = read_html(link)
# 
#     temp <- data.frame(title = get_title(link),
#                        location = get_location(link),
#                        description =get_description(link))
# 
#     data <- rbind(data,temp)
# 
#   }
# 
#   return(data)
# }
# 
# links<- get_links(7)
# 
# links<- unique(links)
# 
# final <- get_info(links)
# writexl::write_xlsx(final,'hire_am.xlsx')
