my_months <- c('JAN','FEB','MARCH','APRIL','MAY','JUNE','JULY','AUG','SEP', 'OCT','NOV','DEC')
library(readxl)
library(writexl)

dfs <- c()

for (i in 2:9) {
  temp <- (paste0('201',i))
  
  if(i==9) {
    for (j in 1:2) {
      name <- (paste0(temp,'_',my_months[j]))
      df_name <- (paste0('ML_files/',name,'.xlsx'))
      assign(name,read_xlsx(df_name))
      dfs <- c(dfs,name)
    }
    next
  }
  
  for (j in 1:12) {
    name <- (paste0(temp,'_',my_months[j]))
    df_name <- (paste0('ML_files/',name,'.xlsx'))
    assign(name,read_xlsx(df_name))
    dfs <- c(dfs,name)
  }
}

for (i in dfs) {
  temp <- get(i)
  temp <- temp[,c(1,5)] # 5 is the 6 column boi
  colnames(temp) = c("names", i)
  assign(i, temp)
}
df <- get(dfs[86])

for (i in dfs) {
  df <- merge(x=df, y=get(i), by = 'names', all.x = T)
}


second_try <- df[2,]
write_xlsx(second_try,path = 'ML_ready/second_try.xlsx')

write_xlsx(df,path = 'ML_ready/full_data.xlsx')
