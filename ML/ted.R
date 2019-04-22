set.seed(5152)
ted_main <- read.csv('ted_main.csv')

library(jsonlite)
formatted_ted_ratings <- gsub("'",'"',ted_main$ratings)

ted_ratings <- purrr::map(formatted_ted_ratings, jsonlite::fromJSON)
