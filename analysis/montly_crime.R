library(astsa)

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
plot(predict(model.hw, 10))

plot(model.hw, predicted.values = predict(model.hw, 10))
