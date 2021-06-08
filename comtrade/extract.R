library(stringr)
library(dplyr)
library(readxl)

data <- read.csv("data.csv")
data$cmdCode <- as.numeric(data$cmdCode)

code <- read_xlsx("6 digit capital codes.xlsx")
code <- t(code)

data1 <- data %>% filter(
  cmdCode %in% code
)

library(writexl)


data2 <- data1[data1$motDesc %in% c("All MOTs"),]
data2 <- data2[data2$ptTitle2 %in% c("World"),]


write_xlsx(data2,"data.xlsx")
