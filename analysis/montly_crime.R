library(astsa)
library(forecast)
library(ggfortify)
library(ggplot2)
library(reshape)

df <- read.csv("data/monthly_total_crime.csv")
crime <- ts(df$CrimeCount, frequency=12, start=c(2008,1))

crime.decomposition <- decompose(crime)
plot(crime.decomposition)
crime.detrended <- crime - crime.decomposition$trend
crime.detrended <- crime.detrended[!is.na(crime.detrended)]
spec.pgram(crime.detrended, taper=0, log="no")

crime.train <- ts(crime[time(crime) < 2016], frequency=12, start=c(2008,1))
trend <- time(crime.train) - 2008
trend.squared <- trend * trend
trend.cubed <- trend.squared * trend


is.olympic.year <- as.integer(time(crime.train) >= 2012 & time(crime.train) < 2013)
reg <- lm(crime.train ~ trend + trend.squared + is.olympic.year)


# Seems like there is quite a bit of predictive capacity on the crime time series.
# The decompose function + periodogram both reveal a bit of information on the crime patterns experienced in London
# We can perform inference on the periodogram to assess the significance/CI for each peak

model.hw <- HoltWinters(crime)
forecast <- predict(model.hw, n.ahead=12, prediction.interval = T, level=0.8)
plot(model.hw, forecast, main="Holt-Winters prediction for total crime")

HWplot<-function(ts_object,  n.ahead=4,  CI=.95,  error.ribbon='green', line.size=1){
  
  hw_object<-HoltWinters(ts_object)
  
  forecast<-predict(hw_object,  n.ahead=n.ahead,  prediction.interval=T,  level=CI)
  
  
  for_values<-data.frame(time=round(time(forecast),  3),  value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
  
  fitted_values<-data.frame(time=round(time(hw_object$fitted),  3),  value_fitted=as.data.frame(hw_object$fitted)$xhat)
  
  actual_values<-data.frame(time=round(time(hw_object$x),  3),  Actual=c(hw_object$x))
  
  
  graphset<-merge(actual_values,  fitted_values,  by='time',  all=TRUE)
  graphset<-merge(graphset,  for_values,  all=TRUE,  by='time')
  graphset[is.na(graphset$dev),  ]$dev<-0
  
  graphset$Fitted<-c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
  
  
  graphset.melt<-melt(graphset[, c('time', 'Actual', 'Fitted')], id='time')
  
  p<-ggplot(graphset.melt,  aes(x=time,  y=value)) + geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill=error.ribbon) + geom_line(aes(colour=variable), size=line.size) + geom_vline(x=max(actual_values$time),  lty=2) + xlab('Time') + ylab('Value') + scale_colour_hue('')
  return(p)
  
}

HWplot(crime, n.ahead=12)
