library(dplyr)
## Sheet 1
clean_data <- readxl::read_xlsx("cleaned_data.xlsx")
data <- readxl::read_xlsx("cleaned_data.xlsx")

data$title2 <- NULL
data <- data[-c(1:25),]

test1 <- left_join(data, clean_data)

## We can in fact bind all the old datas together before left_join for maximal amount of categories applied

length(unique(test2$company))

## Sheet2
clean_data2 <- readxl::read_excel('cleaned_data.xlsx', sheet = 'Company Category')
clean_data2 <- clean_data2[,c(1,2)]
clean_data2$
data2 <- as.data.frame(x = unique(test1$company))
data2$x <- ""
colnames(data2) <- colnames(clean_data2)

test2 <- left_join(data2, clean_data2)
