library(readxl)

data = read_xlsx("third_ts.xlsx")
colnames(data) = c("x1","x2","x3")
data$x2 = NULL
# data[1:13,]
j = c(1)
for(i in 1:9) {
  j = c(j, j[i]+ 13)
}

for(i in 1:9){
  name = "a201"
  temp = data[seq(j[i],j[i+1]),]
  assign(paste0(name,i),temp)
}

for (i in 1:9) {
  name = 'a201'
  name2 = 't_201'
  name_temp = paste0(name,i)
  temp <- get(name_temp)
  temp <- temp$x3[!c(grepl(x = temp$x1,pattern =  "[0-9]"))]
  assign(paste0(name2,i),temp)
}

df = c()
df = c(t_2011,t_2012,t_2013,t_2014,t_2015,t_2016,t_2017,t_2018,t_2019)
df = as.data.frame(df)
writexl::write_xlsx(df,'ts-3.xlsx')
      