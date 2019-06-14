library(readxl)
library(forecast)
library(TSA)

df <- read_excel("data_main.xlsx", sheet = "Data")

df <- df[,-1]
df <- df[-c(1:2),]
colnames(df) <- df[1,]
df <- df[-1,]
df <- df[df$`Code GCR` %in% 'GCI',]
df <- df[df$Attribute %in% 'Value',]

df <- df[,-c(1,3:7)]
df <- df[,-c(154:161)]


AM <- df$Armenia
AM <- rev(AM)
AM <- as.numeric(AM)
ts_AM <- ts(AM, start= 2008, frequency = 1)

ts.plot(ts_AM)
acf(ts_AM)


fit <- auto.arima(ts_AM)
plot(fc <- forecast(fit, h = 6))


