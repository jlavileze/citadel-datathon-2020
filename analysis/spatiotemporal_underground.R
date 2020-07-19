df <- read.csv("data/annual_underground_act.csv", row.names = "station")
head(df)
pca.1 <- prcomp(df)
pca.2 <- prcomp(t(df))

write.csv(pca.1$x, "data/underground_pca_spatial.csv")
write.csv(pca.2$x, "data/underground_pca_temporal.csv")

