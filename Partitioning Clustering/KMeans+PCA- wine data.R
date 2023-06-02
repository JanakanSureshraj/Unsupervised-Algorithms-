#Partitioning Clustering - K Means Algorithm 
#Goal: Detect the type of wine based on 13 chemical analyses as features. 
library(NbClust)
library(MASS)
library(factoextra)

wine<- read.csv(" ")
head(wine)

#Pre-processing data 
str(wine)

data.train<- scale(wine[-1]) #center and scale + without the labels
summary(data.train)

#Before PCA 

#Model Fitting 

#Determine best number of clusters 
#NbClust method 
set.seed(10)
nc<-NbClust(data.train, 
            min.nc=2, max.nc=15, 
            method="kmeans")
table(nc$Best.nc[1, ])
#bar chart 
barplot(table(nc$Best.nc[1, ]), 
        xlab="Number of Clusters", 
        ylab="Number of Criteria", 
        main="Number of Clusters Chosen by 24 Criteria")

#Elbow Method
wss<-0
for (i in 1:15){
  wss[i] <-
    sum(kmeans(data.train, centers=i)$withinss)
}
plot(1:15, wss, type="b", 
     xlab="Number of Clusters", 
     ylab="WSS")

#Both the above methods suggest k=3. Select k=3 as the original dataset also contains 3 classes. 

#Fit the Model 
set.seed(1234)
fit.km1<- kmeans(data.train, 3)
fit.km1

#Internal Evaluation Metrics 
wss1=fit.km$tot.withinss
wss1
bss1=fit.km$betweenss
bss1
#specific info
fit.km1$centers
fit.km1$size

#Clustering Viz
#cluser plot
fviz_cluster(fit.km1, data=data.train)
#drawing parallel coordinates plot to see how variables contributed in each cluster
parcoord(data.train, fit.km1$cluster)
# green cluster contains wine with low flavanoids value, low proanthocyanins value, 
# low hue value. Or black cluster contains wine which has dilution value higher than wine in red cluster.

#Confusion Matrix- dataset has labels 
table.km1<- table(wine$Type, fit.km1$cluster)
table.km1

#After PCA 

pca_result<- prcomp(data.train, scale=FALSE)
pca_result
names(pca_result)

#optional code for computation
pca_result$rotation<--pca_result$rotatio
pca_result$rotation
pca_result$x<--pca_result$x
pca_result$x
pca_result$sdev
(VE <- pca_result$sdev^2)
PVE <- VE / sum(VE)
round(PVE, 3)
varPercent <- PVE*100
barplot(varPercent, xlab='PC', ylab='Percent Variance', names.arg=1:length(varPercent), las=1, ylim=c(0, max(varPercent)), col='gray')

#PCA-applied data for kmeans 
#choosing PC1 and PC2
pca_wine<- as.data.frame(pca_result$x[, 1:2])
pca_wine

#identifying value for k using automated tools
fviz_nbclust(pca_wine, kmeans, method = 'wss')
fviz_nbclust(pca_wine, kmeans, method = 'silhouette')
fviz_nbclust(pca_wine, kmeans, method = 'gap_stat')
#all above methods identify k=3

#kmeans model 
k=3
fit.km2 <- kmeans(pca_wine, centers=3)
fit.km2
fviz_cluster(kmeans_wine, data = pca_wine)
wss2 = fit.km2$tot.withinss
bss2 = fit.km2$betweenss
wss2
bss2

#Confusion Matrix 
table.km2<- table(wine$Type, fit.km2$cluster)
table.km2

#Improvement in model performance after PCA!
#wss-> before=1278.34 after=235.149
#bss-> before=1199.66 after=1186.696
#bss/tss-> before=48.4% after=83.5 %