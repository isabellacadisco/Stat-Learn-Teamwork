library(haven)
library(car)
library(pheatmap)
library(dplyr)
library(factoextra)
library(NbClust)

data = read.csv("qualita_vita_2021.csv")
str(data)

n = nrow(data)
p=ncol(data)
p
data

#standardized dataset
data_std = scale(data[,2:91])
rownames(data_std) <- data$DENOMINAZIONE.CORRENTE

#compute the within sum of squares as a function of n cluster to determine K
fviz_nbclust(data_std, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle

#kmeans k=3
set.seed(123)
k.means.fit <- kmeans(data_std, 3) # k = 3
str(k.means.fit)
res_k_means_3 <- as.data.frame(k.means.fit$cluster)
kmeans3 <- as.data.frame(cbind(data_std,k.means.fit$cluster))
write.csv(cbind(data_std,k.means.fit$cluster),"kmeans3.csv")

#kmeans k=5
set.seed(123)
k.means.fit <- kmeans(data_std, 5) # k = 5
str(k.means.fit)
res_k_means_5 <- as.data.frame(k.means.fit$cluster)
kmeans5 <- as.data.frame(cbind(data_std,k.means.fit$cluster))
write.csv(cbind(data_std,k.means.fit$cluster),"kmeans5.csv")

#-----------------------------------
#isolate names of cities
c1 <- subset(res_k_means_5,k.means.fit$cluster==1)
c1 <- rownames(c1)
c1
#-----------------------------------

# pca from covariance matrix
sigma = cov(data[,2:p])
eigen(sigma)

autoval_cov = eigen(sigma)$values
autovec_cov = eigen(sigma)$vectors

# pca from correlation matrix
rho = cor(data[,2:p])
eigen(rho)

autoval_cor = eigen(rho)$values
autovec_cor = eigen(rho)$vectors

# correlation of the components w/ the original features
corr_comp = matrix(nrow = 90, ncol = 90)
for (i in 1:90){
  corr_comp[,i] = autovec_cor[,i]*sqrt(autoval_cor[i])
}

rownames(corr_comp) = colnames(data[,2:91])
colnames(corr_comp) = 1:90
corr_comp

pheatmap(corr_comp, cluster_rows=FALSE, cluster_cols=FALSE)


#analysis of the eigenvalues
pvarsp = autoval_cor/p
pvarspcum = cumsum(pvarsp)
pvarsp
pvarspcum

#scree plot
plot(autoval_cor, type="b", main="Scree Diagram", xlab="Components", ylab="Eigenvalue")
abline(h=1, lwd=3, col="red")


# projection of the original dataset
# on the first 18 components
data_pca = as.matrix(data[,2:91]) %*% autovec_cor[,1:18]
rownames(data_pca) = data[,1]
colnames(data_pca) = c(1:18)
data_pca

# pca kmeans k=3
set.seed(123)
k.means.fit.pca <- kmeans(data_pca, 3) 
str(k.means.fit.pca)
res_k_means_pca_3 <- as.data.frame(k.means.fit.pca$cluster)
kmeans_pca_3 <- as.data.frame(cbind(data,k.means.fit.pca$cluster))
write.csv(cbind(data_std,k.means.fit$cluster),"kmeans_pca_3.csv")


# pca kmeans k=5
set.seed(123)
k.means.fit.pca <- kmeans(data_pca, 5) # k = 5
str(k.means.fit.pca)
res_k_means_pca_5 <- as.data.frame(k.means.fit.pca$cluster)
kmeans_pca_5 <- as.data.frame(cbind(data,k.means.fit.pca$cluster))
write.csv(cbind(data_std,k.means.fit$cluster),"kmeans_pca_5.csv")


# COMPARISON

comparison = cbind(res_k_means_3, res_k_means_pca_3$`k.means.fit.pca$cluster`)
colnames(comparison) <- c("Original","PCA")
comparison
unmatch <- subset(comparison, Original != PCA)
unmatch


#------------------------------------------------------------------------------

coordinates = read.csv("coordinate.csv")
coordinates


# kmeans 3
kmeans3 <- cbind(kmeans3, coordinates$latitudes,coordinates$longitudes)
write.csv(kmeans3,"kmeans3.csv")

# kmeans 5
kmeans5 <- cbind(kmeans5, coordinates$latitudes,coordinates$longitudes)
write.csv(kmeans5,"kmeans5.csv")

# kmeans pca 3
kmeans_pca_3 <- cbind(kmeans_pca_3, coordinates$latitudes,coordinates$longitudes)
write.csv(kmeans_pca_3,"kmeans_pca_3.csv")

# kmeans pca 5
kmeans_pca_5 <- cbind(kmeans_pca_5, coordinates$latitudes,coordinates$longitudes)
write.csv(kmeans_pca_5,"kmeans_pca_5.csv")























