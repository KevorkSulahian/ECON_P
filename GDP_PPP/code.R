y <- sample(c(0:100), 70, replace = T)
x <- sample(c(10:100), 70, replace = T)
df <- data.frame(x,y)
my_point <- df[8,]
library(ggplot2)
library(ggrepel)
library(ggthemes)

ggplot(df, aes(x = x, y = y)) + geom_point(color = "red", aes(size = y/ 10)) + geom_text_repel(aes(x,y, label = y)) +
  theme_economist() + geom_point(data = my_point, color = "blue", size = 6) +
  scale_size(name = expression(y))
