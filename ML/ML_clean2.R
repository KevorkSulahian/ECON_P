library(readxl)
library(sjmisc)
library(stringr)

my_months <- c('Հունվար', 'Փետրվար',  'Մարտ', 'Ապրիլ', 'Մայիս', 'Հունիս', 'Հուլիս', 'Օգոստոս', 'Սեպտեմբեր', 'Հոկտեմբեր',
               'Նոյեմբեր', 'Դեկտեմբեր')


year = '2018'
dfs <- c()
for (i in 1:12) {
  
  temp <- (paste0(year,'_',my_months[i]))

  df_name <- (paste0(year,'.xls'))
  
  assign(temp,read_xls(df_name, sheet = temp))
  dfs <- c(dfs,temp)
  
}
my_months2 <- c('JAN','FEB','MARCH','APRIL','MAY','JUNE','JULY','AUG','SEP', 'OCT','NOV','DEC')
for (i in 1:12) {
  temp <- get(dfs[i])
  temp <- temp[,c(1,6)] # 6 is the 6 column boi
  colnames(temp) = c("names", dfs[i])
  temp = temp[c(grep("[A-Z]", temp$names))[c(1:3)],]
  # assign(i, temp)
  writexl::write_xlsx(temp, path = paste0("ML_files2/",year, '_', my_months2[i], '.xlsx'))
}





