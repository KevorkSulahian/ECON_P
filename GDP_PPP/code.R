
library(ggplot2)
library(ggrepel)
library(ggthemes)
<<<<<<< HEAD
# library(RColorBrewer)

# data is our main data
# data1 is the data we need 
=======
library(RColorBrewer)
>>>>>>> f50588edd04d916a8738e3c1b760fda1820e2201
data <- readxl::read_xls("pruductivity countries 2017().xls", sheet = 2)
data1 <- readxl::read_xls("pruductivity countries 2017().xls", sheet = 3)

# only the col's we need
data<- data[c(1,2,7)]
# deleting rows with NA's
data <- data[complete.cases(data),]
# make it look beautiful
options(scipen = 999)
# Too big
data <- data[data$Productivity <  90,]
colnames(data) <- c("Country","GDP", "Productivity")
<<<<<<< HEAD
# merg them
df <- merge(data, data1, by = "Country")
# divide by million (easier to see)
=======

df <- merge(data, data1, by = "Country")
>>>>>>> f50588edd04d916a8738e3c1b760fda1820e2201
df$GDP <- df$GDP/1000000
df <- df[c(1:4)]
colnames(df) <- c("Country","GDP", "Productivity", "Abb")
# Armenia different point (different color)
AM <- df[df$Country == "Armenia",]
<<<<<<< HEAD
# delete remaining extra cases
=======
>>>>>>> f50588edd04d916a8738e3c1b760fda1820e2201
df <- df[complete.cases(df),]


ggplot(df, aes(x = GDP, y = Productivity)) + geom_point(color = "black", aes(size = Productivity), pch = 21) +
  scale_size(range = c(0,10)) +
  geom_text_repel(aes(GDP, Productivity, label = Abb)) +
  theme_economist_white() +
  geom_point(data = AM, color = "red", size = 2.5) +
  xlim(c(0,3740232.44)) + ylim(c(0,90)) + coord_flip() + theme(legend.position="none") +
  labs(y = "ՀՆԱ (մլն ԱՄՆ դոլար)", x = "Արտադրողականություն (ԱՄՆ դոլար)")

<<<<<<< HEAD
# not to be used
=======
>>>>>>> f50588edd04d916a8738e3c1b760fda1820e2201
ggplot(df, aes(y = Productivity, x = reorder(Abb, -Productivity))) + geom_bar(stat = 'identity') +
  coord_flip() + scale_color_brewer(palette = "Dark2")
