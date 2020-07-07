library(readr)
library(urca)
library(fpp)

crimes<-read.csv("C:/Users/neera/Documents/MITA/Fall2020/BF/Project/Crimes.csv") 
View(crimes)
head(crimes)
crime_ts <- ts(crimes$Data, start=c(2008,1),frequency = 12)
crime_ts
plot(crime_ts)
summary(crime_ts)
boxplot(crime_ts)

#Decomposition
decomp<-decompose(crime_ts)
decomp
plot(decomp)
decomp$seasonal

#Seasonally Adjusted
temp_sesadjust<-seasadj(decomp)
plot(crime_ts)
lines(temp_sesadjust,col='red')

#Naive
naive_forecast<-naive(crime_ts,12)
naive_forecast
plot(naive_forecast)

#Average Method
average_forecast<-meanf(crime_ts,12)
average_forecast
plot(average_forecast)

#Seasonal Naive Method
Seasonal_naive_forecast<-snaive(crime_ts,12)
Seasonal_naive_forecast
plot(Seasonal_naive_forecast)

#Drift Method ----
drift_forecast<-rwf(crime_ts,12)
drift_forecast
plot(drift_forecast)

#Residual Plots 
seasonal_naive_residual_analysis<-residuals(Seasonal_naive_forecast)
seasonal_naive_residual_analysis
plot(seasonal_naive_residual_analysis)

#Plotting all methods
autoplot(crime_ts) + autolayer(meanf(crime_ts, h=11), series="Mean", PI=FALSE) + 
  autolayer(naive(crime_ts, h=11), series="Naïve", PI=FALSE)+
  autolayer(snaive(crime_ts, h=11), series="Seasonal naïve", PI=FALSE)

histo<-hist(seasonal_naive_residual_analysis,breaks=40,main = "Histogram of residuals")
plot(Seasonal_naive_forecast$fitted[2-5],Seasonal_naive_forecast$residuals[2-5],col=c("red","blue"))
plot(Seasonal_naive_forecast$x[2-5],Seasonal_naive_forecast$residuals[2-5],col=c("red","blue"))
Acf(seasonal_naive_residual_analysis)

#Accuracy 
accuracy(naive_forecast)
accuracy(average_forecast)
accuracy(Seasonal_naive_forecast)

#SMA
ma_fore<-ma(crime_ts, order=1)
plot(ma_fore)

#forecast for accuracy table
ma_forecast<-forecast(ma_fore, h=12)
ma_forecast
accuracy(ma_forecast)

#Moving average
ma3_fore<-ma(crime_ts, order=3)  
lines(ma3_fore,col="red",lwd=3)

ma6_fore<-ma(crime_ts, order=6)
lines(ma6_fore,col="blue",lwd=3)

ma9_fore<-ma(crime_ts, order=9) 
lines(ma9_fore,col="green",lwd=3)

ets_forecast <- ets(crime_ts)
fore_ets<-forecast.ets(ets_forecast, h=12)
fore_ets
plot(fore_ets)

#smoothing
library (fpp)
ses(crime_ts, h=12)
summary(ses(crime_ts, h=12))

ses_fore <- ses(crime_ts, h=12)
ses_residual_ana <- residuals(ses_fore)
plot(ses_residual_ana)

hist(ses_residual_ana)

fit_s<-ses_fore$fitted
fit_s
plot(fit_s[2-5],ses_residual_ana[2-5], col=c("Red","Green"))

actual<-ses_fore$x
plot(actual[2-5],ses_residual_ana[2-5],col=c("Blue","Green"))

Acf(ses_residual_ana)
accuracy(ses_fore)
forecast(ses_fore, h=12)
plot(forecast(ses_fore, h=12))
summary(ses_fore)

#Holt winters
holt<-HoltWinters(crime_ts)
holt_forecast<-forecast(holt, h=12)
holt_forecast
plot(holt_forecast)

res_holt<-residuals(holt)
plot(res_holt)
hist(res_holt)

plot(holt_forecast$fitted[2-5],holt_forecast$residuals[2-5],col=c("red","green"))
plot(holt_forecast$x[2-5],holt_forecast$residuals[2-5],col=c("Green","Black"))
Acf(res_holt)
accuracy(holt_forecast)

forecast(holt, h=12)
plot(forecast(holt, h=12))

# ARIMA
adf.test(crime_ts)
Test=ur.kpss(crime_ts) 
summary(Test)

diff(crime_ts)

Test2=ur.kpss(diff(crime_ts)) 
summary(Test2)

ndiffs(crime_ts)

crimediff<- diff(crime_ts, differences=1)
tsdisplay(crimediff)
