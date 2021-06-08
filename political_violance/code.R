library(readr)
library(tidyverse)
library(stringr)
library("sf")
library("ggplot2")
theme_set(theme_bw())
library("rnaturalearth")
library("rnaturalearthdata")
library(data.table)


data <- read_csv2("2018-02-13-2021-02-18-Caucasus_and_Central_Asia-Armenia.csv")

colnames(data)

length(unique(data$data_id))

# more than half of the data is the same?
data <- data %>%
  distinct(data_id, .keep_all = T)


unique(data$event_type)

unique(data$sub_event_type)

unique(data$actor1)

unique(data$assoc_actor_1)

unique(data$source_scale)

# Lat and long need
summary(data$latitude)

fix_cord <- function(test) {
  test <- as.character(test)
  test <- strsplit(test,"")
  
  for(i in 1:length(test)) {
    temp = unlist(test[i])
    temp1 = paste0(temp[1],temp[2],".")
    temp2 <-paste(temp[3:length(temp)], collapse = "")
    temp3 <- as.numeric(paste0(temp1,temp2))
    test[i] = temp3
  }
  
  test <- unlist(test)
  return(test)
}

data$latitude <- fix_cord(data$latitude)
data$longitude <- fix_cord(data$longitude)


# first jab at map

sites <- st_as_sf(data.frame(longitude = data$longitude,
                             latitude = data$latitude),
                  coords = c("longitude", "latitude"),
                  crs = 4326, agr = "constant")
cord_dens <- setDT(data)[, .N, longitude]

data2 <- merge(x = data,y = cord_dens,by= "longitude")

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data2$N <- min_max_norm(data2$N) * 8

world <- ne_countries(scale='medium',returnclass = 'sf')

# (Armenia <- ggplot(data = world) +
#     geom_sf(fill = "antiquewhite1") +
#     geom_sf(data = sites, size = data2$N, shape = 16, aes(color=data$event_type)) +
#     coord_sf(xlim = c(43, 47), ylim = c(38, 42)) +
#     xlab("Longitude")+ ylab("Latitude")+
#     theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
#                                           size = 0.1), panel.background = element_rect(fill = "aliceblue"), 
#           panel.border = element_rect(fill = NA)))
# 


countries <- subset(world, admin %in% c("Armenia"))

(Armenia <- ggplot(data = countries) +
    geom_sf(fill = "antiquewhite1") +
    geom_sf(data = sites, size = data2$N, shape = 16, aes(color=data$event_type)) +
    xlab("Longitude")+ ylab("Latitude") + labs(color = "Event Types"))


by_events <- data %>%
  group_by(event_type) %>%
  count(event_type) %>%
  arrange(desc(n))

ggplot(by_events, aes(x = event_type, y = n, fill = event_type)) +
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_blank()) +
  labs(title = "Number of events in the region")

# 
# sources <- data %>%
#   group_by(source) %>%
#   count(source) %>%
#   arrange(desc(n))
# View(sources)

# need to extract each unique name and then count

unique_sources <- unique(str_subset(data$source,"^[^;]+$",negate = FALSE))
# make a df with unique source and count

# sum(str_detect(data$source,unique_sources[3]))

sources_count <- data.frame(
  source = unique_sources,
  n = 0
)

for (i in 1:length(unique_sources)) {
  sources_count$n[i] = sum(str_detect(data$source,unique_sources[i]))
}

sources_count <- sources_count[order(sources_count$n,decreasing = T),]

top_10_sources <- sources_count[c(1:10),]

ggplot(top_10_sources, aes(x = source, y = n, fill = source)) +
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_blank()) +
  labs(title = "sources")


# without ministry
top_10_sources2 <- sources_count[c(2:11),]

ggplot(top_10_sources2, aes(x = source, y = n, fill = source)) +
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_blank()) +
  labs(title = "sources")


# class(data$event_date)
data$event_date <- as.Date(data$event_date, format = '%d %B %Y')

data3 <- data
data3$month <- month(data$event_date)
data3$year <- year(data$event_date)

# ggplot(data3, aes(month)) +
#   geom_bar(aes(fill = year))


library(plotly)

data3$year <- as.factor(data3$year)
p <- ggplot(data3, aes(x = month)) + 
  geom_bar(aes(y = ..count.., fill = event_type)) + 
  scale_fill_brewer(palette = "Set3") + 
  ylab("n") + 
  ggtitle("Show precentages in bar chart")

fig <- ggplotly(p)

fig

# TEXT
library(tm)
text <- data$notes
vs <- VectorSource(text)
corpus <- VCorpus(vs)
corpus
inspect(corpus[[2]])

meta(corpus, tag = 'language')

dtm <- DocumentTermMatrix(corpus)
inspect(dtm)

tdm <- TermDocumentMatrix(corpus)
inspect(tdm)

library(stringr)
text <- str_remove_all(text, pattern = "[:punct:]")
text[1]


# try 2

text <- data$notes
vs <- VectorSource(text)
corpus <- VCorpus(vs)
corpus <- tm_map(corpus, removePunctuation)
dtm <- TermDocumentMatrix(corpus,
                          control = list(removeNumbers = T, removePunctuation = T,
                                         stopwords= T, stemming = T))
inspect(dtm)
dtm_mat <- as.matrix(dtm)
freqs <- rowSums(dtm_mat)
df_freqs <- data.frame(terms = rownames(dtm_mat),
                       freq = freqs, stringsAsFactors = F)
df_freqs <- df_freqs[order(df_freqs$freq, decreasing = T),]
head(df_freqs)

library(RColorBrewer)
df_freqs_10 <- df_freqs[1:10,]
ggplot(df_freqs_10, aes(x = reorder(terms, freq), y = freq)) +
  geom_bar(stat="identity", fill = brewer.pal(n = 10, name="Spectral")) +
  coord_flip() + labs(x = "Terms", y = "Frequency", title = "top 10 words")

set.seed(1)
library(wordcloud)

wordcloud(words = df_freqs$terms, freq = df_freqs$freq, min.freq = 10,
          max.words = 200, random.order = F,
          colors = brewer.pal(12, 'Dark2'))


tdm <- TermDocumentMatrix(corpus,
                          control = list(removeNumbers = T, removePunctuation = T,
                                         stopwords= T, stemming = T, 
                                         weighting = weightTfIdf))
tdm_mat <- as.matrix(tdm)
freqs <- rowSums(tdm_mat)
df_freqs <- data.frame(terms = rownames(tdm_mat),
                       freq = freqs, stringsAsFactors = F)
df_freqs <- df_freqs[order(df_freqs$freq, decreasing = T),]

set.seed(1)

wordcloud(words = df_freqs$terms, freq = df_freqs$freq, min.freq = 1,
          max.words = 200, random.order = F,
          colors = brewer.pal(8, 'Dark2'))


