# importing libraries
library(stats)
library(NbClust)
library(cluster)
library(mclust)
library(readr)
library(dplyr)

setwd("~/MSDS ML Project")

# downloading the data to dataframe
df <- read.csv("Mobile_clust.csv")
df
sample = df[seq(150,200, by = 5),]
sample_scaled <- scale(sample)
sample_scaled

distMatrix_E <- dist(sample_scaled, method="euclidean")
distMatrix_Mi <- dist(sample_scaled, method="minkowski", p =3)
distMatrix_M <- dist(sample_scaled, method="manhattan")

## Euclidean
groups_E <- hclust(distMatrix_E,method="ward.D")
plot(groups_E, cex=0.9, hang=-1, main = "Euclidean")
rect.hclust(groups_E, k=3)

## Minkowski
groups_Mi <- hclust(distMatrix_Mi,method="ward.D")
plot(groups_Mi, cex=0.9, hang=-1, main = "Minkowski")
rect.hclust(groups_E, k=3)

## Manhattan
groups_M <- hclust(distMatrix_M,method="ward.D")
plot(groups_M, cex=0.9, hang=-1, main = "Manhattan")
rect.hclust(groups_E, k=3)

#Cosine
similarity_matrix <- tcrossprod(scale(sample_scaled, center = TRUE, scale = TRUE))

# perform hierarchical clustering using cosine similarity
hclust_results <- hclust(as.dist(1 - similarity_matrix), method = "ward.D2")

# plot the dendrogram
plot(hclust_results, main = "Hierarchical Clustering using Cosine Similarity")




