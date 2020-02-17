

setwd('C:/Users/kalyj/OneDrive/RGU/MSc Data Science/CMM507Group2/research_dev_georgios')
plastic1 <- read.csv("TWAP_LME_Plastics_ModelDistribution.csv")
str(plastic1)
head(plastic1)
summary(plastic1)

names(plastic1)
View(plastic1)

#average weight of microplastic piece
microwc <- plastic1$micro..Weight.Density..g.km2./plastic1$micro..Count.Density..counts.km2.
boxplot(microwc, ylab="grams per piece")

#average weight of macroplastic piece
macrowc <- plastic1$macro..Weight.Density..g.km2./plastic1$macro..Count.Density..counts.km2.
plot(macrowc, ylab="grams per piece")

plastic1$macroWoverC <- macrowc
plastic1$microWoverC <- microwc

maw <- plastic1$macro..Weight.Density..g.km2.
mac <- plastic1$macro..Count.Density..counts.km2.
mawc <- plastic1$macroWoverC

plot(plastic1$micro..Count.Density..counts.km2.,plastic1$micro..Weight.Density..g.km2.)
plot(mac,maw)
plot(mac, mawc)
plot(maw, mawc)

#higher counts = higher weight
#appox 3 areas have high count but average weight/count
#appox 2 areas have high weight/count though low count (Black & Yellow sea)

outlier <- subset(plastic1, plastic1$macroWoverC > 2.5)
outlier

cor(mac,mawc)
cor(maw,mawc)
cor(maw,mac)




madata <- plastic1[,5:6]

# K-Means Clustering with 5 clusters
fit <- kmeans(madata, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster)
clusplot(madata, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
install.packages("fpc")
library(fpc)
plotcluster(madata, fit$cluster)

