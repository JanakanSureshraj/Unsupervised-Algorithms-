#Partitioning Clustering - K Means Algorithm 
#Goal: Cluster based on 18 attributes and 4 classes of vehicles. 

library(NbClust)
library(cluster)
library(factoextra)
library(cluster)
library(clValid)
library(stats)

data<- read.csv(" ")
head(data)
str(data)

#preprocessing tasks
#1- scaling 
scale<- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

data_norm<- as.data.frame(lapply(data[2:19], scale))
head(data_norm)

#2- outlier-removal
boxplot(data_norm)

#8 variables named Rad.Ra, Pr.Axis.Ra, Max.L.Ra, Sc.Var.Maxis, 
#Sc.Var.maxis, Skew.Maxis, Skew.maxis, Kurt.maxis contain outliers.

is_outlier <- function(x) {
  quartiles = quantile(x, probs = c(0.25, 0.75), na.rm =FALSE)
  
  lower_quartile <- quartiles[1]
  upper_quartile <- quartiles[2]
  IQR = upper_quartile-lower_quartile
  
  extreme.threshold.upper = (IQR * 1.5) + upper_quartile
  extreme.threshold.lower = lower_quartile - (IQR * 1.5)
  
  # Return logical vector
  x > extreme.threshold.upper | x < extreme.threshold.lower
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    cat("Removing outliers in column: ", col, " \n")
    df <- df[!is_outlier(df[[col]]),]
  }
  df
}

variables_with_outliers<- c('Rad.Ra', 'Pr.Axis.Ra', 'Max.L.Ra', 'Sc.Var.Maxis', 
                            'Sc.Var.maxis', 'Skew.Maxis', 'Skew.maxis', 'Kurt.maxis')
data_no_outliers<- remove_outliers(data_norm, variables_with_outliers)
data_no_outliers
dim(data_no_outliers)
boxplot(data_no_outliers)

#Further removal - one outlier
data_preprocessed<- subset(data_no_outliers, data_no_outliers$Sc.Var.maxis<0.975)

#data viz
boxplot(data_preprocessed)
dim(data_preprocessed)

#determining the number of clusters using automated tools 
#NbClust
set.seed(10)
clustNo<-NbClust(data_preprocessed, distance = "euclidean", min.nc=2, max.nc=8,
             method = "centroid", index = "all")
barplot(table(clustNo$Best.n[1,]),    
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 24 Criteria")

#Elbow 
k=2:10
set.seed(42)
WSS= sapply(k, function(k){kmeans(data_preprocessed, centers=k)$tot.withinss})
plot(k, WSS, type="b", xlab="Number of clusters", ylab="Within Sum of Squares")
#alternatively- fviz_nbclust(data_preprocessed, kmeans, method = 'wss')

#gap statistics
fviz_gap_stat(clusGap(data_preprocessed, FUNcluster = stats::kmeans, K.max = 15, nstart = 50))

#Silhouette 
fviz_nbclust(data_preprocessed, FUNcluster = stats::kmeans, method='silhouette')

#K-Means on the number of clusters calculated by the above tools 
k=4
kmeans= kmeans(data_preprocessed, centers=k, nstart=10)
kmeans
fviz_cluster(kmeans, data = data_preprocessed)

#silhouette coefficient (another internal eval. metric)
sil <- silhouette(kmeans$cluster, dist(data_preprocessed))
fviz_silhouette(sil)

#Principle Component Analysis (PCA)
pca_data<- prcomp(data_preprocessed, scale=FALSE)

pca_data$rotation <- -pca_data$rotation
pca_data$rotation

summary(pca_data)
#PC 1,2,3,4 and 5 can be selected (cum. var. ~95%).  

#transforming dataset based on PCs
data_transformed<- as.data.frame(-pca_data$x[, 1:5])
head(data_transformed)

#determining the number of clusters using automated tools for the transformed dataset
#NbClust
set.seed(10)
clustNo<-NbClust(data_transformed, distance = "euclidean", min.nc=2, max.nc=8,
                 method = "centroid", index = "all")
barplot(table(clustNo$Best.n[1,]),    
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 24 Criteria")

#Elbow 
k=2:10
set.seed(42)
WSS= sapply(k, function(k){kmeans(data_transformed, centers=k)$tot.withinss})
plot(k, WSS, type="b", xlab="Number of clusters", ylab="Within Sum of Squares")

#gap statistics
fviz_gap_stat(clusGap(data_transformed, FUNcluster = stats::kmeans, K.max = 15))

#Silhouette 
fviz_nbclust(data_transformed, stats::kmeans, method='silhouette')

#K-Means on the number of clusters calculated by the above tools 
k_new=4
kmeans_new= kmeans(data_transformed, centers=k_new, nstart=10)
kmeans_new
fviz_cluster(kmeans_new, data = data_transformed)

#silhouette coefficient (another internal eval. metric)
sil <- silhouette(kmeans_new$cluster, dist(data_transformed))
fviz_silhouette(sil)

#Calinski-Harabasz (internal evaluation metric)
#Many ways to compute. This is one. 
ch2 <- c()
for (k in 2:5) {
  pam.res <- pam(data_transformed, k)
  ch2[k-1] <- pam(data_transformed, k)$criterion
}
plot(2:5, ch2, type = "b", xlab = "Number of clusters", 
     ylab = "Calinski-Harabasz index", main="Calinski-Harabasz Index")

