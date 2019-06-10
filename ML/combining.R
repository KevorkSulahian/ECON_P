my_months <- c('JAN','FEB','MARCH','APRIL','MAY','JUNE','JULY','AUG','SEP', 'OCT','NOV','DEC')
library(readxl)
for (i in 2:9) {
  temp <- (paste0('201',i))
  
  if(i==9) {
    for (j in 1:2) {
      name <- (paste0(temp,'_',my_months[j]))
      df_name <- (paste0('ML_files/',name,'.xlsx'))
      assign(name,read_xlsx(df_name))
    }
    next
  }
  
  for (j in 1:12) {
    name <- (paste0(temp,'_',my_months[j]))
    df_name <- (paste0('ML_files/',name,'.xlsx'))
    assign(name,read_xlsx(df_name))
  }
}
