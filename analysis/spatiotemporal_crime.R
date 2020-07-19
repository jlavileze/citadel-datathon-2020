library(ggfortify)
library(forecast)

df <- read.csv("data/crime_by_borough.csv", row.names = "Borough")
pca.1 <- prcomp(df)
pca.2 <- prcomp(t(df))

pca.1$x

write.csv(pca.1$x, "data/crime_pca_spatial.csv")
write.csv(pca.2$x, "data/crime_pca_temporal.csv")

signif(pca.1$x[,1:3], 2)

pc1.ts <- ts(as.vector(pca.2$x[,1]), frequency=12, start=c(2008,1))
pc2.ts <- ts(as.vector(pca.2$x[,2]), frequency=12, start=c(2008,1))
pc3.ts <- ts(as.vector(pca.2$x[,3]), frequency=12, start=c(2008,1))

pc1.decomp <- decompose(-pc1.ts)
pc1.detrended <- pc1.decomp$x - pc1.decomp$trend
pc1.detrended <- pc1.detrended[!is.na(pc1.detrended)]

autoplot(-pc1.ts)
autoplot(decompose(-pc1.ts))

-pc1.ts %>% decompose %>% autoplot
spec.pgram(pc1.detrended, taper=0, log="no")

# How do I pretty plot spectrogram?

# PC1: Level
hw.pc1 <- HoltWinters(-pc1.ts)
forecast <- predict(hw.pc1, n.ahead=12, level = 0.8)
plot(hw.pc1, forecast, main="Holt-Winters prediction for crime spatiotemporal PC1")

# PC2: Funding
hw.pc2 <- HoltWinters(-pc2.ts)
forecast <- predict(hw.pc2, n.ahead=12, level = 0.8)
plot(hw.pc2, forecast, main="Holt-Winters prediction for crime spatiotemporal PC2")

# PC3: Media excitement (peaks in 2012 and 2018 correlated to Olympics and post-Brexit)
hw.pc3 <- HoltWinters(-pc3.ts)
forecast <- predict(hw.pc3, n.ahead=12, level = 0.8)
plot(hw.pc3, forecast, main="Holt-Winters prediction for crime spatiotemporal PC3")

## Let's regress the east series on PC1/2/3

east <- read.csv("data/east_avg_crime.csv")
east <- ts(east$East, frequency=12, start=c(2008,1))

#m <- lm(east ~ -pc1.ts + -pc2.ts + -pc3.ts)
m <- lm(east ~ -pc1.ts)
summary(m)
crime.east.pc <- data.frame(east, -pc1.ts, -pc2.ts, -pc3.ts)
colnames(crime.east.pc) <- c("east", "pc1", "pc2", "pc3")
crime.east.pc.scaled <- data.frame(scale(crime.east.pc))

m <- lm(east ~ pc1 + pc2 + pc3, data=crime.east.pc.scaled)
summary(m)
