set.seed(5152)
ted_main <- read.csv('ted_main.csv')

library(jsonlite)
formatted_ted_ratings <- gsub("'",'"',ted_main$ratings)

ted_ratings <- purrr::map(formatted_ted_ratings, jsonlite::fromJSON)

for (i in (1:length(ted_ratings))) {
  ted_ratings_df <- ted_ratings[[i]]
  highest_rating_count <- ted_ratings_df[which(ted_ratings_df$count == max(ted_ratings_df$count)), ]
  ted_main$highest_rating[i] <- highest_rating_count$name
}
ted_main$highest_rating = as.factor(ted_main$highest_rating)

trainObs <- sample(nrow(ted_main), .6 * nrow(ted_main), replace = FALSE)
testObs <- sample(nrow(ted_main), .4 * nrow(ted_main), replace = FALSE)
train_dat <- ted_main[trainObs,]
test_dat <- ted_main[testObs,]

library(tm)

train_corpus <- VCorpus(VectorSource(train_dat$transcript))
##Removing Punctuation
train_corpus <- tm_map(train_corpus, content_transformer(removePunctuation))
##Removing numbers
train_corpus <- tm_map(train_corpus, removeNumbers)
##Converting to lower case
train_corpus <- tm_map(train_corpus, content_transformer(tolower))
##Removing stop words
train_corpus <- tm_map(train_corpus, content_transformer(removeWords), stopwords("english"))
##Stemming
train_corpus <- tm_map(train_corpus, stemDocument)
##Whitespace
train_corpus <- tm_map(train_corpus, stripWhitespace)
# Create Document Term Matrix
dtm_train <- DocumentTermMatrix(train_corpus)
train_corpus <- removeSparseTerms(dtm_train, 0.4)
dtm_train_matrix <- as.matrix(train_corpus)
dtm_train_matrix <- cbind(dtm_train_matrix, train_dat$highest_rating)
colnames(dtm_train_matrix)[ncol(dtm_train_matrix)] <- "y"
training_set_ted_talk <- as.data.frame(dtm_train_matrix)
training_set_ted_talk$y <- as.factor(training_set_ted_talk$y)


library(caret)
review_ted_model <- train(y ~., data = training_set_ted_talk, method = 'svmLinear3')
# Preparing our test data. It’s the same repetitive procedure.
test_corpus <- VCorpus(VectorSource(test_dat$transcript))
##Removing Punctuation
test_corpus <- tm_map(test_corpus, content_transformer(removePunctuation))
##Removing numbers
test_corpus <- tm_map(test_corpus, removeNumbers)
##Converting to lower case
test_corpus <- tm_map(test_corpus, content_transformer(tolower))
##Removing stop words
test_corpus <- tm_map(test_corpus, content_transformer(removeWords), stopwords("english"))
##Stemming
test_corpus <- tm_map(test_corpus, stemDocument)
##Whitespace
test_corpus <- tm_map(test_corpus, stripWhitespace)
# Create Document Term Matrix
dtm_test <- DocumentTermMatrix(test_corpus)
test_corpus <- removeSparseTerms(dtm_test, 0.4)
dtm_test_matrix <- as.matrix(test_corpus)


#Build the prediction 
model_ted_talk_result <- predict(review_ted_model, newdata = dtm_test_matrix)
check_accuracy <- as.data.frame(cbind(prediction = model_ted_talk_result, rating = test_dat$highest_rating))
library(dplyr)
check_accuracy <- check_accuracy %>% mutate(prediction = as.integer(prediction) - 1)
check_accuracy$accuracy <- if_else(check_accuracy$prediction == check_accuracy$rating, 1, 0)
round(prop.table(table(check_accuracy$accuracy)), 3)
library(performanceEstimation)
classificationMetrics(as.integer(test_dat$highest_rating), model_ted_talk_result)
most_common_misclassified_ratings = check_accuracy %>% filter(check_accuracy$accuracy == 0) %>%
  group_by(rating) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(3)
##Most commong missclassified rating
levels(train_dat$highest_rating)[most_common_misclassified_ratings$rating]

