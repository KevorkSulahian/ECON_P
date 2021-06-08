
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

data <- readxl::read_xlsx("gdp-prod 2019.xlsx")
data1 <- readxl::read_xls("pr_ductivity countries 2018.xls", sheet = 3)
data2 <- readxl::read_xls("eeu_eu .xls")
colnames(data) <- c("Abb", "Productivity", "GDP")




df <- merge(data, data1,  by.x = "Abb", by.y = '...2')

df <- df[c(1,2,3,4)]
colnames(df) <- c("Abb","Productivity", "GDP", "Country")
AM <- df[df$Country == "Armenia",]


colnames(data2) <- c("Country", "Union")
df <- merge(df, data2, by = "Country", all.x = T )


df$Union[is.na(df$Union)] <- "other"
options(scipen = 9999)
df <- df[complete.cases(df),]
df$Productivity <- as.numeric(as.character(df$Productivity))
df$GDP <- as.numeric(as.character(df$GDP))
df$Productivity = df$Productivity/100000
AM$Productivity = AM$Productivity/100000
df <- df[df$Productivity <  40,]


scatter <- ggplot(df, aes(x = GDP, y = Productivity)) +
  geom_point(aes(size = Productivity, col = Union)) +
  scale_size(range = c(0,10),
             name = "Արտադրողականություն")+
  geom_text_repel(aes(GDP, Productivity, label = Abb), na.rm = T) +
  geom_point(data = AM, color = "red", size = 3.5) +
  ylim(c(0,30)) + xlim(c(0,60)) +
  coord_flip()  +
  labs(x = "ՀՆԱ (մլն ԱՄՆ դոլար)", y = "Արտադրողականություն (ԱՄՆ դոլար-ժամ)", legend = " Արտադրողականություն",
       col = "")

scatter + theme_bw() + scale_color_manual(values=c('#999999','#ce8108','#00c7c7')) + 
  theme( legend.text = element_text(size = 10),
         legend.title = element_text(size = 10)) +
         # legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical") +
  guides(colour = guide_legend(override.aes = list(size=5)))

ggsave("import3.png", width = 10, height = 10, dpi = 250, units = "in")
# return(here2)
dev.off()
