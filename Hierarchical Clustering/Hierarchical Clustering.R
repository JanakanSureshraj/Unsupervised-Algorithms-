#Hierarchical clustering (HC) does not require to pre-specify the number of clusters to be produced. 
#HC can be subdivided into 2 groups: 
#1. Agglomerative (bottom-up)- each observation is considered as a cluster of its own (leaf) and the most similar clusters are merged until there is just one single big cluster (root) left.
#2. Divisive (top-down)- inverse of agg. Begins with the root in which all objects are included in one cluster. Most heterogeneous clusters are divided until each observation is in its own cluster.
#Results of HC is a tree-based representation of the objects, which is also known as dendrograms. 

#Goal: hierarchical clustering on the USArrests dataset.

library(factoextra)
library(igraph)
library(dendextend)
library(corrplot)
library(cluster)
library(NbClust)

data("USArrests")
df<-scale(USArrests)
head(df)

#computing the distancne matrix using the Euclidean distances
res.dist<-dist(df, method="euclidean") #default method is euclidean
as.matrix(res.dist)[1:6, 1:6] 

#now, the clusters computed above will be linked to each other to create bigger clusters. 
#this process is iterated until all objects in the original data are linked together in a hierarchical tree. 

res.hc1<- hclust(d=res.dist, method="complete") #complete(max) linkage agglomeration method is used here.
#other common methods: single(min), average, centroid and Ward’s minimum variance method: It minimizes the total within-cluster variance. 

#dendrogram 
fviz_dend(res.hc1, cex=0.5)
#the height in the dendrogram is known as the cophenetic distance between the two objects.

#checking if the distances in the tree reflect the original distances correctly. 
#computing the cophenetic distance 
res.coph1<-cophenetic(res.hc1)   
#correlation between cophenetic distance and the original distance 
cor(res.dist, res.coph1)
#the closer the value of the correlation coefficient is to 1, the more accurately the clustering solution reflects your data. 

#using other linkage methods 

res.hc2<-hclust(res.dist, method="average")
cor(res.dist, cophenetic(res.hc2))

res.hc3<-hclust(res.dist, method="single")
cor(res.dist, cophenetic(res.hc3))

res.hc4<-hclust(res.dist, method="ward.D2")
cor(res.dist, cophenetic(res.hc4))

#the average linkage has a higher correlation coefficient and it creates a tree that represents the original distances better. 

#since HC does not tell the number of clusters, the dendrogram needs to be cut into different groups or clusters. 
#We can cut the hierarchical tree at a given height in order to partition our data into clusters. 
grp<-cutree(res.hc1, k=4) #can inspect the clustering solution at different k values. 
head(grp)
#number of members in each cluster 
table(grp)
#interpret as: we have 4 clusters and the first one has 8 states. 
#getting the names of the members in cluster 1
rownames(df)[grp==1]

#finding the mean of the variables in each cluster. 
final_data<-cbind(USArrests, cluster=grp) #merging with orignal dataset
head(final_data)
aggregate(final_data, by=list(cluster=final_data$cluster), mean)
#intepret as: the mean number of murders per 100,000 citizens among the states in cluster 1 is 14.08.

#clustering visualization

#viz 1- vertical dendrogram 
fviz_dend(res.hc1, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # colour labels by groups
          rect = TRUE # Add rectangle around groups
)

#viz 2- horizontal dendogram
fviz_dend(res.hc1, k=4, cex=0.4, horiz=TRUE,k_colors = "jco", rect = TRUE, rect_border = "jco", rect_fill = TRUE)

#viz 3- scatter plot 
fviz_cluster(list(data = df, cluster = grp),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

#viz 4- circular 
fviz_dend(res.hc1, cex = 0.5, k = 4, k_colors = "jco", type = "circular")

#viz 5- phylogenic-like tree
fviz_dend(res.hc1, k = 4, k_colors = "jco", type = "phylogenic", repel = TRUE)

#Comparing Dendrograms 
dend1<-as.dendrogram(res.hc1)
dend2<-as.dendrogram(res.hc2)
dend3<-as.dendrogram(res.hc3)
dend4<-as.dendrogram(res.hc4)

dend_list<-dendlist("Complete"=dend1, "Average"=dend2, "Single"=dend3, "Ward.D2"=dend4)

tanglegram(dend1, dend2)
#customize it 
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colours
           common_subtrees_color_branches = TRUE, # Colour common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

#correlation matrix between a list of dendrograms
#the function cor.dendlist() can be used to compute the “Cophenetic” correlation matrix between a list of trees. 
#the value can range between -1 to 1. With near 0 values meaning that the two trees are not statistically similar.
cor_cophenetic(dend1, dend2)

cors<-cor.dendlist(dend_list)
round(cors, 2)

corrplot(cors, "pie", "lower")
#For example, we can see that there is a higher correlation between complete and ward.D2 linkage methods.

#Internal Evaluation Metirc- Silhouette Method 
sil<-silhouette(grp, res.dist)
fviz_silhouette(sil)
# silhouette coefficients close to +1 (best value, very well clustered) means the sample is far away from the neighbouring 
# clusters. A value of 0 means that the sample is on or very close to the decision boundary between two neighbouring clusters 
# (i.e. overlapping clusters). Finally, negative values indicate that the samples could have potentially been assigned to the 
# wrong cluster. Some researchers also mention that data with negative value sometimes could be considered as outliers, thus they 
# need to be removed from our analysis.
head(sil)
#identifying the names of the samples with negative sil coefficients. 
neg_sil_index <-  which(sil[,  'sil_width']  <  0)
neg_sil_index
sil[neg_sil_index, ]
final_data[neg_sil_index, ]#the states with negative sil score 

#Further, NbClust Method can be used to identify the number of clusters before using the cutree command. 
nb = NbClust(df, method = "complete")  #other methods= single, average, ward.D2, centroid 

