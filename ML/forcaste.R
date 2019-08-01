require(R.utils)
require(signal)
library(forecast)
library(TSA)
library(readxl)
library(zoo)
library(tseries)
library(forecast)







df <- read_excel("ML_ready/first_try.xlsx")


df$names <- NULL

ts <- as.numeric(df)
ts <- ts(ts, start = c(2014,10), frequency = c(12))

ts.plot(ts)

ts3 <- decompose(na.StructTS(ts))$x
ts.plot(ts3)

acf(ts3)

decomposedRes <-decompose(ts3, type="additive")
plot (decomposedRes)



plot(ts3)

abline(reg=lm(ts3~time(ts3)))

#This will print the cycle across years.
cycle(ts3)


#This will aggregate the cycles and display a year on year trend
plot(aggregate(ts3, FUN = mean))

#Box plot across months will give us a sense on seasonal effect
boxplot(ts3~cycle(ts3))

adf.test(diff(log(ts3)), alternative="stationary", k=0)

acf(log(ts3))


par(mfrow=c(1,2))
acf(diff(log(ts3)))
pacf(diff(log(ts3)))



stlRes <- stl(ts3, s.window = "periodic")

(fit_log <- arima(log(ts3), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))
pred <- predict(fit, n.ahead = 10*12)

fit <- auto.arima(ts3)
plot(fc <- forecast(fit, h = 6))


# None of these worked so far

ts3_adjusted <- ts3 - decomposedRes$seasonal
plot(decomposedRes)
plot(ts3)
plot(ts3_adjusted)

ts3_adjusted_forcast <- HoltWinters(ts3_adjusted, beta = F, gamma = F)
ts3_adjusted_forcast
ts3_adjusted_forcast$fitted
plot(ts3_adjusted_forcast)
ts3_adjusted_forcast$SSE

ts3_adjusted_forcast2 <- forecast:::forecast.HoltWinters(ts3_adjusted_forcast, h=3)
ts3_adjusted_forcast2$mean # the answer is here
autoplot(ts3_adjusted_forcast2)

acf(ts3_adjusted_forcast2$residuals, na.action = na.pass, lag.max=20)

Box.test(ts3_adjusted_forcast2$residuals, lag=20, type="Ljung-Box")

plot.ts(ts3_adjusted_forcast2$residuals)


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors,na.rm = T)/4
  mysd   <- sd(forecasterrors, na.rm = T)
  mymin  <- min(forecasterrors,na.rm = T) - mysd*5
  mymax  <- max(forecasterrors, na.rm = T) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(ts3_adjusted_forcast2$residuals)

###
###
### Aparently this means that we all good because the mean is close to 0
###
###

# 
# The Ljung-Box test showed that there is little evidence of non-zero autocorrelations
# in the in-sample forecast errors, and the distribution of forecast errors seems to
# be normally distributed with mean zero. This suggests that the simple exponential
# smoothing method provides an adequate predictive model for London rainfall, which
# probably cannot be improved upon. Furthermore, the assumptions that the 80% and 95%
#   predictions intervals were based upon (that there are no autocorrelations in the
# forecast errors, and the forecast errors are normally distributed with mean
# zero and constant variance) are probably valid.

# Holt-Winters Exponential Smoothing

ts3_log <- log(ts3)
ts4 <- HoltWinters(ts3_log)
ts4$fitted
ts4$SSE

# The estimated values of alpha, beta and gamma are 0.41, 0.00, and 0.96,
# respectively. The value of alpha (0.41) is relatively low, indicating that
# the estimate of the level at the current time point is based upon both recent
# observations and some observations in the more distant past. The value of beta is 0.00,
# indicating that the estimate of the slope b of the trend component is not
# updated over the time series, and instead is set equal to its initial value.
# This makes good intuitive sense, as the level changes quite a bit over
# the time series, but the slope b of the trend component remains roughly the same.
# In contrast, the value of gamma (0.96) is high, indicating that the estimate of
# the seasonal component at the current time point is just based upon very recent observations.

plot(ts4)
# red is predicted I think

ts4_forcast <- forecast:::forecast.HoltWinters(ts4, h=5)
forecast:::plot.forecast(ts4_forcast)

acf(ts4_forcast$residuals, na.action = na.pass, lag.max=20)
Box.test(ts4_forcast$residuals, lag=20, type="Ljung-Box")

plot.ts(ts4_forcast$residuals)
plotForecastErrors(ts4_forcast$residuals)
#












































## df[7] with Full Data not deleting Na's



df2 <- read_excel("ML_ready/second_try.xlsx")


# df$names <- NULL
df2 <- df2[,-c(1,2)]
ts2 <- as.numeric(df2)
decompose(na.StructTS(ts2))$x
ts2 <- ts(ts2, start = c(2012,1), frequency = c(12))

## the first value of the time series must not be missing

decompose(na.StructTS(ts2))$x


ts.plot(ts2)

# acf(ts2)



fit2 <- auto.arima(ts2)
plot(fc2 <- forecast(fit2, h = 6))


## ok some shit has gotten done nigga
## use full Data with the data boi




first_month_forcast <- c(0)

for (i in 1:nrow(data)) {
  temp = as.vector(data[i,])
  temp <- as.numeric(temp)
  temp = ts(temp, start = c(2012,1), frequency = c(12))
  temp <- auto.arima(temp)
  first_month_forcast[i] <- as.numeric(forecast(temp, h = 1)[[4]])
}

data$first_forcast <- round(first_month_forcast)


#####
##### BIG BOI FORCASTE
# GOTTA LOOP MY BOY



