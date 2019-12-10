my_months <- c('JAN','FEB','MARCH','APRIL','MAY','JUNE','JULY','AUG','SEP', 'OCT','NOV','DEC')
library(readxl)
library(writexl)

dfs <- c()

for (i in 6:8) {
  temp <- (paste0('201',i))
  
  if(i==9) {
    for (j in 1:6) { ### change here for 2019
      name <- (paste0(temp,'_',my_months[j]))
      df_name <- (paste0('ML_files2/',name,'.xlsx'))
      assign(name,read_xlsx(df_name))
      dfs <- c(dfs,name)
    }
    next
  }
  
  for (j in 1:12) {
    name <- (paste0(temp,'_',my_months[j]))
    df_name <- (paste0('ML_files2/',name,'.xlsx'))
    assign(name,read_xlsx(df_name))
    dfs <- c(dfs,name)
  }
}

for (i in dfs) {
  temp <- get(i)
  # temp <- temp[,c(1,5)] # 5 is the 6 column boi
  colnames(temp) = c("names", i)
  assign(i, temp)
}
df <- get(dfs[36]) # change here too

for (i in dfs) {
  df <- merge(x= df, y= get(i), by = 'names', all.x = T)
}


second_try <- df[2,]
write_xlsx(second_try,path = 'ML_ready/second_try.xlsx')

write_xlsx(df,path = 'ML_ready/full_data2.xlsx')





data <- read_excel("ML_ready/full_data2.xlsx")

data_names <- data$names

# data <- data[,-c(1,2)]

# data[] <- lapply(data, function(x) as.numeric(x))

data[is.na(data)] <- 0


data2 <- t(data)
rows <- rownames(data2)
colnames(data2) <- data_names
data$names = NULL
data2 <- as.data.frame(data2)
# data2$months = rows
writexl::write_xlsx(data2, 'ML_ready/data_ML.xlsx')
