---
title: "tsf_export"
author: "Kevork Sulahian"
date: "October 28, 2019"
output: pdf_document
---


```r
library(readxl)
library(forecast)
```


```r
# library(readxl)

df <- read_xlsx("Export_for_TS.xlsx")
```

```
## New names:
## * `` -> ...2
```

```r
df = df[1,]
df = df[-c(1,2)]

df2 = read_xlsx('export_19xlsx.xlsx')
```

```
## New names:
## * `` -> ...1
```

```r
df = t(df)
# df2$...1 = c(paste0(2019,"-",1:12))
rownames(df2) =df2$...1
```

```
## Warning: Setting row names on a tibble is deprecated.
```

```r
df2$...1 = NULL
colnames(df2) = "V1"
df = as.data.frame(df)

df3 = rbind(df,df2)
ts = ts(df3,start=c(2009,1), frequency = c(12))
```

![](tsf_export_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

In this case, it appears that an additive model is not appropriate for describing this time series, since the size of the seasonal fluctuations and random fluctuations seem to increase with the level of the time series. Thus, we may need to transform the time series in order to get a transformed time series that can be described using an additive model. For example, we can transform the time series by calculating the natural log of the original data:


```r
log_ts <- log(ts)
plot.ts(log_ts)
```

![](tsf_export_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 


##Decomposing Time Series

Decomposing a time series means separating it into its constituent components, which are usually a trend component and an irregular component, and if it is a seasonal time series, a seasonal component.

###Decomposing Seasonal Data
A seasonal time series consists of a trend component, a seasonal component and an irregular component. Decomposing the time series means separating the time series into these three components: that is, estimating these three components.


```r
ts_components <- decompose(ts)
```
we can print out the estimated values of the seasonal component 

```r
ts_components$seasonal
```

```
##              Jan         Feb         Mar         Apr         May         Jun
## 2009 -35868.9712  -5110.0251    583.1592  -4870.7687  -7353.4162   7568.9602
## 2010 -35868.9712  -5110.0251    583.1592  -4870.7687  -7353.4162   7568.9602
## 2011 -35868.9712  -5110.0251    583.1592  -4870.7687  -7353.4162   7568.9602
## 2012 -35868.9712  -5110.0251    583.1592  -4870.7687  -7353.4162   7568.9602
## 2013 -35868.9712  -5110.0251    583.1592  -4870.7687  -7353.4162   7568.9602
## 2014 -35868.9712  -5110.0251    583.1592  -4870.7687  -7353.4162   7568.9602
## 2015 -35868.9712  -5110.0251    583.1592  -4870.7687  -7353.4162   7568.9602
## 2016 -35868.9712  -5110.0251    583.1592  -4870.7687  -7353.4162   7568.9602
## 2017 -35868.9712  -5110.0251    583.1592  -4870.7687  -7353.4162   7568.9602
## 2018 -35868.9712  -5110.0251    583.1592  -4870.7687  -7353.4162   7568.9602
## 2019 -35868.9712  -5110.0251    583.1592  -4870.7687  -7353.4162   7568.9602
## 2020 -35868.9712                                                            
##              Jul         Aug         Sep         Oct         Nov         Dec
## 2009   4706.4842   2153.8132   8726.0894   8919.6430   8748.6388  11796.3933
## 2010   4706.4842   2153.8132   8726.0894   8919.6430   8748.6388  11796.3933
## 2011   4706.4842   2153.8132   8726.0894   8919.6430   8748.6388  11796.3933
## 2012   4706.4842   2153.8132   8726.0894   8919.6430   8748.6388  11796.3933
## 2013   4706.4842   2153.8132   8726.0894   8919.6430   8748.6388  11796.3933
## 2014   4706.4842   2153.8132   8726.0894   8919.6430   8748.6388  11796.3933
## 2015   4706.4842   2153.8132   8726.0894   8919.6430   8748.6388  11796.3933
## 2016   4706.4842   2153.8132   8726.0894   8919.6430   8748.6388  11796.3933
## 2017   4706.4842   2153.8132   8726.0894   8919.6430   8748.6388  11796.3933
## 2018   4706.4842   2153.8132   8726.0894   8919.6430   8748.6388  11796.3933
## 2019   4706.4842   2153.8132   8726.0894   8919.6430   8748.6388  11796.3933
## 2020
```

![](tsf_export_files/figure-latex/unnamed-chunk-8-1.pdf)<!-- --> 
The plot above shows the original time series (top), the estimated trend component (second from top), the estimated seasonal component (third from top), and the estimated irregular component (bottom)


## Seasonally Adjusting

```r
ts_seasonall <- ts - ts_components$seasonal
```

![](tsf_export_files/figure-latex/unnamed-chunk-10-1.pdf)<!-- --> 

## Holt-Winters Exponential Smoothing

```r
ts_forcaste <- HoltWinters(ts)
ts_forcaste
```

```
## Holt-Winters exponential smoothing with trend and additive seasonal component.
## 
## Call:
## HoltWinters(x = ts)
## 
## Smoothing parameters:
##  alpha: 0.3550801
##  beta : 0.01223594
##  gamma: 0.3718725
## 
## Coefficients:
##            [,1]
## a   227306.8042
## b     1573.4174
## s1   -7376.7400
## s2     343.7398
## s3   -7858.7490
## s4  -11901.5253
## s5    5357.7476
## s6    7156.3868
## s7    6538.0354
## s8    4815.7484
## s9    8180.6545
## s10   8364.9546
## s11   1449.0512
## s12 -50544.5232
```

```r
#
```

The value of alpha (0.35) is relatively low, indicating that the estimate of the level at the current time point is based upon both recent observations and some observations in the more distant past. The value of beta is 0.01, indicating that the estimate of the slope b of the trend component is updated but doesn't have much effect over the time series, and instead is set equal to its initial value. This makes good intuitive sense, as the level changes quite a bit over the time series, but the slope b of the trend component remains roughly the same. In contrast, the value of gamma (0.38) is high, indicating that the estimate of the seasonal component at the current time point is not just based upon very recent observations


```r
ts_forcaste$SSE
```

```
## [1] 23479926065
```

![](tsf_export_files/figure-latex/unnamed-chunk-13-1.pdf)<!-- --> 



```r
ts_forcaste2 = forecast:::forecast.HoltWinters(ts_forcaste, h= 11)
(as.data.frame(ts_forcaste2))[1]
```

```
##          Point Forecast
## Feb 2020       221503.5
## Mar 2020       230797.4
## Apr 2020       224168.3
## May 2020       221698.9
## Jun 2020       240531.6
## Jul 2020       243903.7
## Aug 2020       244858.8
## Sep 2020       244709.9
## Oct 2020       249648.2
## Nov 2020       251405.9
## Dec 2020       246063.4
```


![](tsf_export_files/figure-latex/unnamed-chunk-15-1.pdf)<!-- --> 
## Growth


```r
year_2019 <- window(ts, 2019,c(2019,12))
sum_year_2019 = sum(c(year_2019))
year_2020 = (as.data.frame(ts_forcaste2))[1][c(1:11),]
sum_year_2020_HW <-sum(c(year_2020),window(ts, 2020))
growth_HW <- growth(sum_year_2020_HW,sum_year_2019)
growth_HW
```

```
## [1] 0.06859268
```

We can investigate whether the predictive model can be improved upon by checking whether the in-sample forecast errors show non-zero autocorrelations at lags 1-20, by making a correlogram and carrying out the Ljung-Box test:

![](tsf_export_files/figure-latex/unnamed-chunk-17-1.pdf)<!-- --> 

```
## 
## 	Box-Ljung test
## 
## data:  ts_forcaste2$residuals
## X-squared = 12.859, df = 20, p-value = 0.8834
```
The correlogram shows that the autocorrelations for the in-sample forecast errors do not exceed the significance bounds for lags 1-20. Furthermore, the p-value for Ljung-Box test is 0.9, indicating that there is no evidence of non-zero autocorrelations at lags 1-20.


We can check whether the forecast errors have constant variance over time, and are normally distributed with mean zero, by making a time plot of the forecast errors and a histogram (with overlaid normal curve):


```r
plot.ts(ts_forcaste2$residuals)
```

![](tsf_export_files/figure-latex/unnamed-chunk-18-1.pdf)<!-- --> 



```r
# plotForecastErrors(ts_forcaste2$residuals)
```
From the time plot, it appears plausible that the forecast errors have constant variance over time. From the histogram of forecast errors, it seems plausible that the forecast errors are normally distributed with mean zero.

Thus,there is little evidence of autocorrelation at lags 1-20 for the forecast errors, and the forecast errors appear to be normally distributed with mean zero and constant variance over time. This suggests that Holt-Winters exponential smoothing provides an adequate predictive model of the log of total productivity, which probably cannot be improved upon. Furthermore, the assumptions upon which the prediction intervals were based are probably valid.




```r
plot.ts(ts)
```

![](tsf_export_files/figure-latex/unnamed-chunk-20-1.pdf)<!-- --> 

```r
ts_diff1 <-  diff(ts, differences = 1)

plot.ts(ts_diff1)
```

![](tsf_export_files/figure-latex/unnamed-chunk-20-2.pdf)<!-- --> 
The time series of differences (above) does appear to be stationary in mean and variance, as the level of the series stays roughly constant over time, and the variance of the series appears roughly constant over time



```r
acf(ts_diff1, lag.max=20)             # plot a correlogram
```

![](tsf_export_files/figure-latex/unnamed-chunk-21-1.pdf)<!-- --> 
We see from the correlogram that the autocorrelation exceeds the significance bound 3 times but all the others do not exceed


```r
acf(ts_diff1, lag.max=20, plot=FALSE) # get the autocorrelation values
```

```
## 
## Autocorrelations of series 'ts_diff1', by lag
## 
## 0.0000 0.0833 0.1667 0.2500 0.3333 0.4167 0.5000 0.5833 0.6667 0.7500 0.8333 
##  1.000 -0.390 -0.066  0.041  0.049 -0.091  0.068 -0.188  0.113  0.079 -0.159 
## 0.9167 1.0000 1.0833 1.1667 1.2500 1.3333 1.4167 1.5000 1.5833 1.6667 
## -0.199  0.505 -0.141 -0.154  0.109  0.003 -0.035  0.029 -0.129  0.046
```


```r
pacf(ts_diff1, lag.max=20)             # plot a partial correlogram
```

![](tsf_export_files/figure-latex/unnamed-chunk-23-1.pdf)<!-- --> 

```r
pacf(ts_diff1, lag.max=20, plot=FALSE) # get the partial autocorrelation values
```

```
## 
## Partial autocorrelations of series 'ts_diff1', by lag
## 
## 0.0833 0.1667 0.2500 0.3333 0.4167 0.5000 0.5833 0.6667 0.7500 0.8333 0.9167 
## -0.390 -0.258 -0.116  0.005 -0.074  0.015 -0.222 -0.078  0.060 -0.110 -0.382 
## 1.0000 1.0833 1.1667 1.2500 1.3333 1.4167 1.5000 1.5833 1.6667 
##  0.270  0.216 -0.110  0.006  0.046 -0.042 -0.082  0.025 -0.054
```

# Arima, 0,1,0


```r
ts_arima = Arima(ts, order=c(1,1,1),seasonal = list(order = c(1,1,1)))
ts_arima
```

```
## Series: ts 
## ARIMA(1,1,1)(1,1,1)[12] 
## 
## Coefficients:
##          ar1      ma1     sar1     sma1
##       0.0199  -0.6250  -0.1612  -0.7249
## s.e.  0.1551   0.1227   0.1207   0.1065
## 
## sigma^2 estimated as 178190944:  log likelihood=-1314.33
## AIC=2638.66   AICc=2639.19   BIC=2652.6
```



```r
ts_arima_forecast = forecast(ts_arima,h = 11)
ts_arima_forecast
```

```
##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
## Feb 2020       217750.2 200638.1 234862.4 191579.4 243921.0
## Mar 2020       226899.4 208501.1 245297.6 198761.7 255037.0
## Apr 2020       218520.2 198990.3 238050.1 188651.8 248388.7
## May 2020       214394.1 193795.9 234992.3 182891.8 245896.4
## Jun 2020       235437.7 213824.0 257051.5 202382.3 268493.1
## Jul 2020       233673.6 211089.9 256257.3 199134.8 268212.4
## Aug 2020       236843.1 213329.5 260356.8 200882.1 272804.1
## Sep 2020       237924.8 213516.6 262332.9 200595.7 275253.9
## Oct 2020       246072.2 220801.1 271343.3 207423.4 284721.0
## Nov 2020       244826.2 218720.7 270931.6 204901.3 284751.0
## Dec 2020       246327.3 219413.3 273241.3 205165.9 287488.8
```

```r
forecast:::plot.forecast(ts_arima_forecast)
```

![](tsf_export_files/figure-latex/unnamed-chunk-25-1.pdf)<!-- --> 

## Growth

```r
# this_year_predict_ARIMA <- (as.data.frame(ts_arima_forecast))[1]

# growth_ARIMA <- growth(sum(c(this_year,as.numeric(this_year_predict_ARIMA$`Point Forecast`))), sum(last_year))
# growth_ARIMA


# year_2019_predict_ARIMA <- (as.data.frame(ts_arima_forecast))[1][c(1),]
# sum_year_2019 = sum(c(year_2019))
year_2020 = (as.data.frame(ts_arima_forecast))[1][c(1:11),]
sum_year_2020_ARIMA = sum(c(year_2020,window(ts,2020)))
growth_ARIMA <- growth(sum_year_2020_ARIMA,sum_year_2019)
growth_ARIMA 
```

```
## [1] 0.04534847
```
As in the case of exponential smoothing models, it is a good idea to investigate whether the forecast errors of an ARIMA model are normally distributed with mean zero and constant variance, and whether the are correlations between successive forecast errors.

For example, we can make a correlogram of the forecast errors for our ARIMA(0,1,1) model, and perform the Ljung-Box test for lags 1-20, by typing:



```r
acf(ts_arima_forecast$residuals, lag.max=20)
```

![](tsf_export_files/figure-latex/unnamed-chunk-27-1.pdf)<!-- --> 

```r
Box.test(ts_arima_forecast$residuals, lag=20, type="Ljung-Box")
```

```
## 
## 	Box-Ljung test
## 
## data:  ts_arima_forecast$residuals
## X-squared = 14.401, df = 20, p-value = 0.8096
```
# p value too high to reject


```r
plot.ts(ts_arima_forecast$residuals)            # make time plot of forecast errors
```

![](tsf_export_files/figure-latex/unnamed-chunk-28-1.pdf)<!-- --> 

```r
plotForecastErrors(ts_arima_forecast$residuals)
```

![](tsf_export_files/figure-latex/unnamed-chunk-28-2.pdf)<!-- --> 

# A model chosen automatically


```r
fit3 <- auto.arima(ts)
fit3
```

```
## Series: ts 
## ARIMA(1,0,1)(0,1,1)[12] with drift 
## 
## Coefficients:
##          ar1      ma1     sma1      drift
##       0.9348  -0.5736  -0.8165  1208.7731
## s.e.  0.0435   0.0962   0.0938   177.2516
## 
## sigma^2 estimated as 172154591:  log likelihood=-1323.51
## AIC=2657.03   AICc=2657.55   BIC=2671.01
```

```r
fit_forecast = forecast(fit3,h=11)
plot(fit_forecast)
```

![](tsf_export_files/figure-latex/unnamed-chunk-29-1.pdf)<!-- --> 

```r
# str(fit)
```

## Growth

```r
# year_2019 <- window(ts, 2019)
# year_2019_predict_HW <- (as.data.frame(ts_forcaste2))[1][c(1),]
# sum_year_2019 = sum(c(year_2019))
# year_2020 = (as.data.frame(ts_forcaste2))[1][c(2:13),]


# year_2019_predict_auto.arima <- (as.data.frame(fit_forecast))[1][c(1),]
# year_2019_predict_auto.arima_95_low <- (as.data.frame(fit_forecast))[4][c(1),]
# year_2019_predict_auto.arima_95_high <- (as.data.frame(fit_forecast))[5][c(1),]

# sum_year_2019 = sum(c(year_2019))
# sum_year_2019_low = sum(c(year_2019,year_2019_predict_auto.arima_95_low))
# sum_year_2019_high = sum(c(year_2019,year_2019_predict_auto.arima_95_high))


year_2020_predict_auto.arima <- (as.data.frame(fit_forecast))[1][c(1:11),]
# year_2020_predict_auto.arima_95_low <- (as.data.frame(fit_forecast))[4][c(2:13),]
# year_2020_predict_auto.arima_95_high <- (as.data.frame(fit_forecast))[5][c(2:13),]

sum_year_2020_auto.ARIMA = sum(c(year_2020_predict_auto.arima,window(ts,2020)))

growth_auto.arima <- growth(sum_year_2020_auto.ARIMA,sum_year_2019)
# growth_auto.arima_95_low <- growth(sum(year_2020_predict_auto.arima_95_low),sum_year_2019_low)
# growth_auto.arima_95_high <- growth(sum(year_2020_predict_auto.arima_95_high),sum_year_2019_high)

growth_auto.arima
```

```
## [1] 0.03690604
```

```r
# growth_auto.arima_95_low
# growth_auto.arima_95_high
```


  






