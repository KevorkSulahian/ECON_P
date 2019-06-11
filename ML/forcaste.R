df <- read_excel("ML_ready/first_try.xlsx")


df$names <- NULL

ts <- as.numeric(df)
ts <- ts(ts, start = c(2014,10), frequency = c(12))

ts.plot(ts)

acf(ts)


library(forecast)
library(TSA)

fit <- auto.arima(ts)
plot(fc <- forecast(fit, h = 6))

## with Full Data 
df2 <- read_excel("ML_ready/second_try.xlsx")


df$names <- NULL
df2 <- df2[,-c(1,2)]
ts2 <- as.numeric(df2)
ts2 <- ts(ts2, start = c(2012,1), frequency = c(12))

ts.plot(ts2)

acf(ts2)



fit2 <- auto.arima(ts2)
plot(fc2 <- forecast(fit2, h = 6))


## ok some shit has gotten done nigga
## use full Data with the data boi
