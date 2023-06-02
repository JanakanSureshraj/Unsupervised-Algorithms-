#Partitioning Clustering - K Means Algorithm 
#Goal: Detect species in the Iris dataset upon performing PCA 

library(tidyverse)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(dataset)

data(iris)
df<- cbind(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width)

pca_iris = prcomp(df, center=TRUE, scale=TRUE) #scale the df for PCA
summary(pca_iris)
#sd is the eigenvalue of each PC, prop var is how much a PC accounts for the variance in the dataset and cum var is how much combined PCs account for the var in the dataset. 

#PC1 and PC2 have a cum prop of ~96 so they can be selected. 
iris_transform<- as.data.frame(-pca_iris$x[, 1:2])
head(iris_transform)
#dataset has 2 attributes now!

#using automated tools to identify the ideal k 

#Elbow Method 
wss<- function(k){
  kmeans(iris_transform, k, nstart=10)$tot.withinss
}
k.values<- 1:15
wss_values<- map_dbl(k.values, wss)
plot(k.values, wss_values, 
     type="b", pch=19, frame=FALSE, 
     xlab="Number of Clusters k", 
     ylab="Total Within Sum of Squares")
#alternatively 
fviz_nbclust(iris_transform, kmeans, method="wss")

#Average Silhouette Method 
#A high average silhouette width indicates a good clustering.
fviz_nbclust(iris_transform, kmeans, method="silhouette")

#Gap Statistics Algorithm
# The gap statistic compares the total intra-cluster variation for different values 
# of k with their expected values under zero-mean reference distribution of the data 
# (i.e. a distribution with no obvious clustering).
fviz_nbclust(iris_transform, kmeans, method = 'gap_stat')

#Based on the above results, k is chosen as 3. 
k =3
kmeans_iris = kmeans(iris_transform, centers=3, nstart=10)
kmeans_iris

#Visualization 
#Cluster Plot 1
fviz_cluster(kmeans_iris, data=iris_transform)

#Cluster Plot 2
fviz_cluster(kmeans_iris, data=iris_transform, ellipse.type="euclid",
             star.plot=TRUE, repel=TRUE, ggtheme=theme_minimal())

#identifying which cluster each sample has fallen under 
iris_cluster<-data.frame(iris, cluster=as.factor(kmeans_iris$cluster))
head(iris_cluster)

#Internal Evaluation Metrics 

#Within Sum of Squares (WSS)- indicates cohesion. Similarity of objects in one cluster. Lower the better.
#Between Sum of Squares (BSS)- indicates seperation. How dissimilar one cluster is from another. Higher the better.  
wss=kmeans_iris$tot.withinss
bss=kmeans_iris$betweenss
wss
bss

#Silhouette Method- another internal eval metric
sil<- silhouette(kmeans_iris$cluster, dist(iris_transform))
fviz_silhouette(sil)
#sil plot- 3 clusters with adequate size (data is distributed well among the 3 clusters), width of each cluster > avg sil width and this is a healthy indicator. 

