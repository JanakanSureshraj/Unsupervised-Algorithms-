#Goal of PCA- represent the data in a low-dimensional space. 
#PCA minimizes complexity in model training, making it quicker. 
#Both manual and automated computations of PCA. 
#Datasets: USArrests (four variables that represent the number of arrests per 100,000 residents for Assault, Murder, and Rape in each of the fifty US states in 1973).

install.packages('tidyverse')
library(tidyverse)
install.packages('gridExtra')
library(gridExtra)
install.packages('ggplot')
library(ggplot2)
install.packages('rlang')
library(rlang) 

data("USArrests")
head(USArrests, 10)

#variance for each column/feature
apply(USArrests,2,var)#1- row, 2- col

#scale function to standardize each variable
scaled_df<- scale(USArrests)
head(scaled_df)

#calculating principal components manually!

#calculating covariance matrix
arrests_cov<- cov(scaled_df)
#calculating eignevalues and eigenvector matrix
arrests_eigen<- eigen(arrests_cov)
str(arrests_eigen)

#taking the first two sets of loadings (PC1 and PC2) and sort in the matrix
(phi<- arrests_eigen$vectors[,1:2]) #,[]- all rows

#eigenvectors in R point in the negative direction. Negate to point in positive direction. 
phi<- -phi
row.names(phi)<- c("Murder", "Assault", "UrbanPop", "Rape")
#set of loadings for the first principle component and second. 
colnames(phi)<- c("PC1", "PC2")
phi

#projecting the data points onto the first eigenvector 
#these projected values are called the principal components scores for each row/observation
PC1<- as.matrix(scaled_df)%*%phi[,1]
PC2<- as.matrix(scaled_df)%*%phi[,2]
#df with principal component values
PC<- data.frame(State= row.names(USArrests), PC1, PC2)
head(PC)

#plotting the first and second pc for each US state in a two-dimensional view
ggplot(PC, aes(PC1, PC2))+
         modelr::geom_ref_line(h=0)+
         modelr::geom_ref_line(v=0)+
         geom_text(aes(label=State), size=3)+
         xlab("First Principal Component")+
         ylab("Second Principal Component")+
         ggtitle("First Two Principle Components of USArrests Data")

#A vector of proportion of variance for each component is calculated
PVE<- arrests_eigen$values/sum(arrests_eigen$values)
round(PVE, 2)

#plot PVE
PVEplot <- qplot(c(1:4), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1)
PVEplot
#cumulative PVE plot 
cumPVE<- qplot(c(1:4), cumsum(PVE))+
                 geom_line()+
                 xlab("Principle Component")+
                 ylab("Null")+
                 ggtitle("Cumulative Scree Plot")+
                 ylim(0, 1)
cumPVE
grid.arrange(PVEplot, cumPVE, ncol=2)

#calculated PCA manually so far

#using the BUILT IN prcomp function to perform PCA. prcomp centers variables to mean zero. 
data("USArrests")
head(USArrests, 10)
apply(USArrests, 2, var)
pca_result<- prcomp(USArrests, scale=TRUE)#scale=TRUE sets the standard deviation to one. 
names(pca_result) 
summary(pca_result)

#center and scale components correspond to the means and standard deviations of the variables that were used for scaling prior to implementing PCA. 
pca_result$center
pca_result$scale

#rotation matrix- principal component loadings- each col of the rotation contains the corresponding principal component loading vector. 
#The values in this loadings matrix represent the coefficients used to calculate the principal components from the original variables.
pca_result$rotation

#eigenvectors in R point in the negative direction, as when computed manually. 
pca_result$rotation<- -pca_result$rotation
pca_result$rotation

#We can also obtain the principal components scores from our results as these are stored in the x list item of our results
pca_result$x<- pca_result$x
head(pca_result$x)

#plotting components 3 and 4 within biplot 
biplot(pca_result, choices=3:4, scale=0)
#scale=0- ensures arrows are scaled to represent the loadings. 

#standard deviation
pca_result$sdev

#square sd for variance
ve<- pca_result$sdev^2
ve

#computing the proportion of variance explained by each component by dividing each component's variance by the total variance. 
pve<- ve/sum(ve)
round(pve, 2) #interpretation- first pc has a 62% variance in data, 

#Plotting the pve and the cumulative pve as done earlier
varPercent<- pve*100 
barplot(varPercent, xlab="PC", ylab="Percent Variance", names.arg=1:length(varPercent), las=1, ylim=c(0, max(varPercent)), col="gray")
abline(h=1/ncol(USArrests)*100, col="red")

sqrt(1/ncol(USArrests))

#Graphical representation
install.packages('factoextra')
library(factoextra)
fviz_contrib(pca_result, choice="var", axes=1)

#The prcomp() will use singular value decomposition (SVD) of the data matrix, not by using eigen on the covariance matrix. This is a preferred method for numerical accuracy. 
#The princomp() will use eigen on the correlation or covariance matrix. 
new_pca<- princomp(~Murder + Assault + UrbanPop+ Rape, data= USArrests, cor=TRUE)
summary(new_pca, loadings=TRUE)
predict(new_pca)
screeplot(new_pca, type="lines")
biplot(new_pca)


