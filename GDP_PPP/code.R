
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(scales)
library(ggthemes)
library(grid)
library(devtools)

# data is our main data
# data1 is the data we need 
library(RColorBrewer)

data <- readxl::read_xls("pr_ductivity countries 2018.xls", sheet = 2)
data1 <- readxl::read_xls("pr_ductivity countries 2018.xls", sheet = 3)
data2 <- readxl::read_xls("eeu_eu .xls")
colnames(data2) <- c("Country", "Union")
# only the col's we need
data<- data[c(1,2,7)]
# deleting rows with NA's
data <- data[complete.cases(data),]
# make it look beautiful
options(scipen = 999)

# Too big
data <- data[data$Productivity <  90,]
colnames(data) <- c("Country","GDP", "Productivity")

# merg them
df <- merge(data, data1, by.x = "Country", by.y = 'Country...1')
# divide by million (easier to see)
df <- merge(data, data1,  by.x = "Country", by.y = 'Country...1')
df$`GDP ppp` <- df$`GDP ppp`/1000000

df <- df[c(1,4,5,6)]
colnames(df) <- c("Country","Abb", "Productivity", "GDP")
# Armenia different point (different color)
AM <- df[df$Country == "Armenia",]

# delete remaining extra cases

df <- df[complete.cases(df),]
df <- merge(df, data2, by = "Country", all.x = T )
# df$Union <- gsub("^EU", "եՄ", df$Union)
# df$Union <- gsub("^EEU", "ԵԱՏՄ", df$Union)

df$Union[is.na(df$Union)] <- "other"
options(scipen = 9999)


scatter <- ggplot(df, aes(x = GDP, y = Productivity)) + geom_point(aes(size = Productivity, col = Union)) +
  scale_size(range = c(0,10),
             name = "Արտադրողականություն")+
  geom_text_repel(aes(GDP, Productivity, label = Abb), na.rm = T) +
  geom_point(data = AM, color = "red", size = 2.5) +
  xlim(c(0,3740232.44)) + ylim(c(0,90)) +
  coord_flip()  +
  labs(x = "ՀՆԱ (մլն ԱՄՆ դոլար)", y = "Արտադրողականություն (ԱՄՆ դոլար)", legend = " Արտադրողականություն",
       col = "")

scatter + theme_bw() + scale_color_manual(values=c('#999999','#ce8108','#00c7c7')) + 
  theme( legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical") +
  guides(colour = guide_legend(override.aes = list(size=5)))

ggsave("import2.png", width = 10, height = 10, dpi = 250, units = "in")
# return(here2)
dev.off()


