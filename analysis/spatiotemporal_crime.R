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

pc1.decomp <- decompose(-pc1.ts)
pc1.detrended <- pc1.decomp$x - pc1.decomp$trend
pc1.detrended <- pc1.detrended[!is.na(pc1.detrended)]

autoplot(-pc1.ts)
autoplot(decompose(-pc1.ts))

-pc1.ts %>% decompose %>% autoplot
spec.pgram(pc1.detrended, taper=0, log="no")

# How do I pretty plot spectrogram?