library(ggfortify)
library(forecast)

df <- read.csv("data/crime_by_borough.csv", row.names = "Borough")
pca.1 <- prcomp(df)
pca.2 <- prcomp(t(df))

sum(pca.1$sdev[1:3]^2)/sum(pca.1$sdev^2)
sum(pca.2$sdev[1:3]^2)/sum(pca.2$sdev^2)

write.csv(pca.1$x, "data/crime_pca_spatial.csv")
write.csv(pca.2$x, "data/crime_pca_temporal.csv")

signif(pca.1$x[,1:3], 2)

pc1.ts <- ts(as.vector(pca.2$x[,1]), frequency=12, start=c(2008,1))
pc2.ts <- ts(as.vector(pca.2$x[,2]), frequency=12, start=c(2008,1))
pc3.ts <- ts(as.vector(pca.2$x[,3]), frequency=12, start=c(2008,1))


pc1.decomposition <- decompose(-pc1.ts)
pc2.decomposition <- decompose(-pc2.ts)
pc3.decomposition <- decompose(-pc3.ts)

pc.trends <- data.frame(pc1.decomposition$trend, pc2.decomposition$trend, pc3.decomposition$trend)

write.csv(pc.trends, "data/crime_pc_trends.csv")



pc1.decomp <- decompose(-pc1.ts)
pc1.detrended <- pc1.decomp$x - pc1.decomp$trend
pc1.detrended <- pc1.detrended[!is.na(pc1.detrended)]

plot(pc1.decomp$trend)

autoplot(-pc1.ts)
autoplot(decompose(-pc1.ts))

-pc1.ts %>% decompose %>% autoplot
-pc2.ts %>% decompose %>% autoplot
-pc3.ts %>% decompose %>% autoplot
spec.pgram(pc1.detrended, taper=0, log="no")

plot(-pc1.ts, -pc2.ts, -pc3.ts)

# How do I pretty plot spectrogram?

# PC1: Level
hw.pc1 <- HoltWinters(ts(scale(-pc1.ts), frequency=12, start=c(2008,1)))
forecast.pc1 <- predict(hw.pc1, n.ahead=12, level = 0.8, prediction.interval = T)
plot(hw.pc1, forecast.pc1, main="Holt-Winters prediction for crime spatiotemporal PC1")

# PC2: Funding
hw.pc2 <- HoltWinters(ts(scale(-pc2.ts), frequency=12, start=c(2008,1)))
forecast.pc2 <- predict(hw.pc2, n.ahead=12, level = 0.8, prediction.interval = T)
plot(hw.pc2, forecast.pc2, main="Holt-Winters prediction for crime spatiotemporal PC2")

# PC3: Media excitement (peaks in 2012 and 2018 correlated to Olympics and post-Brexit)
hw.pc3 <- HoltWinters(ts(scale(-pc3.ts), frequency=12, start=c(2008,1)))
forecast.pc3 <- predict(hw.pc3, n.ahead=12, level = 0.8, prediction.interval = T)
plot(hw.pc3, forecast.pc3, main="Holt-Winters prediction for crime spatiotemporal PC3")

## Let's regress the east series on PC1/2/3

east <- read.csv("data/east_avg_crime.csv")
east <- ts(east$East, frequency=12, start=c(2008,1))

#m <- lm(east ~ -pc1.ts + -pc2.ts + -pc3.ts)
m <- lm(east ~ -pc1.ts)
summary(m)
crime.east.pc <- data.frame(east, -pc1.ts, -pc2.ts, -pc3.ts)
colnames(crime.east.pc) <- c("east", "pc1", "pc2", "pc3")
crime.east.pc.scaled <- data.frame(scale(crime.east.pc))

google.trends <- read.csv("data/london_google_trends_clean.csv")
head(google.trends)
crime.east.pc.scaled$GoogleTrends <- google.trends$GoogleTrendsInterest

head(crime.east.pc.scaled)

m <- lm(east ~ pc1 + pc2 + pc3 + GoogleTrends, data=crime.east.pc.scaled)
summary(m)
st(east)
st
east.factor.forecast <- m$coefficients[2] * forecast.pc1[,1] + m$coefficients[3] * forecast.pc2[,1] + m$coefficients[4] * forecast.pc3[,1]

east.factor.forecast.descaled <- sd(east) * east.factor.forecast + mean(east)

plot(east.factor.forecast.descaled)
ts(east.factor.forecast.descaled, frequency = 12, start=c(2019,1))

plot(east, ts(east.factor.forecast.descaled, frequency = 12, start=c(2019,1)))

plot(east)

library(xts)

ts1<-as.xts(east)
ts2<-as.xts(east.factor.forecast.descaled)
ts3 <- c(ts1, ts2)
ts3
plot(ts3, main="Average monthly crime in East boroughs")
events <- xts(c("Olympics", "Forecast"), as.Date(c("2012-07-01", "2019-01-01")))
addEventLines(events, srt=90, pos=2)
events



write.csv(crime.east.pc, "data/temporal_pcs.csv")
