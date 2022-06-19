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

# Elbow methos
#compute the within sum of squares as a function of n cluster to determine K
fviz_nbclust(data_std, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle

# before pca -------------------------------------------------------

#kmeans k=3
set.seed(123)
k.means.fit <- kmeans(data_std, 3) # k = 3
str(k.means.fit)

#results
res_k_means_3 <- as.data.frame(k.means.fit$cluster)

#kmeans k=5
set.seed(123)
k.means.fit <- kmeans(data_std, 5) # k = 5
str(k.means.fit)

res_k_means_5 <- as.data.frame(k.means.fit$cluster)

#hierarchical

d <- dist(data_std, method="euclidean")
h.fit <- hclust(d, method="ward.D2")
plot(h.fit)
#abline(h=15, col="red")

#k =3
groups_3 <- cutree(h.fit, k=3) # cut a 3 cluster
rect.hclust(h.fit, k=3, border = "red")

#k=5
groups_5 <- cutree(h.fit, k=5)
rect.hclust(h.fit, k=5, border = "red")




#----------------------------------- AGGLOMERATION PROGRAM


## function for agglomeration program 
agglo <- function(hc){
  data.frame(row.names=paste0("Cluster",seq_along(hc$height)),
             height=hc$height,
             components=ifelse(hc$merge<0, 
                               hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge)),
             stringsAsFactors=FALSE) }


#h1<-hclust(ds, method="average")
#agglo(h1)


# agglo 
agglo(h.fit)

#-----------------------------------

# save results
coordinates = read.csv("coordinate.csv")

write.csv(cbind(coordinates, res_k_means_3, res_k_means_5, groups_3, groups_5), "to_map.csv")


#---------------------------------------------------------------------


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

corr_comp[,1]


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

data_pca_st = scale(data_pca)




# pca kmeans k=3
set.seed(123)
k.means.fit.pca <- kmeans(data_pca_st, 3) 
str(k.means.fit.pca)

res_k_means_pca_3 <- as.data.frame(k.means.fit.pca$cluster)

# pca kmeans k=5
set.seed(123)
k.means.fit.pca <- kmeans(data_pca_st, 5) # k = 5
str(k.means.fit.pca)

res_k_means_pca_5 <- as.data.frame(k.means.fit.pca$cluster)

# pca hierarchical
d_pca <- dist(data_pca_st, method="euclidean")
h.fit.pca <- hclust(d_pca, method = "ward.D2")

plot(h.fit.pca)

groups_pca_3 <- cutree(h.fit.pca, k=3)
rect.hclust(h.fit.pca, k=3, border="red")

groups_pca_5 <- cutree(h.fit.pca, k=5)
rect.hclust(h.fit.pca, k=5, border="blue")

# agglomeration
agglo(h.fit)
agglo(h.fit.pca)

# add results to csv
write.csv(cbind(coordinates, res_k_means_pca_3, res_k_means_pca_5, groups_pca_3, groups_pca_5), "to_map_pca.csv")


# ranking su prime due componenti
# subset indicatori correlati con prime due componenti

col1<- corr_comp[,1]
pos_cor1<- col1[col1>0.6]
neg_cor1<- col1[col1<(-0.6)]
pos_cor1
neg_cor1

col2<- corr_comp[,2]
pos_cor2<- col2[col2>0.6]
neg_cor2<- col2[col2<(-0.6)]
pos_cor2
neg_cor2



#------------------------------------------------------------------------------

#cose inutili

# COMPARISON

comparison = cbind(res_k_means_3, res_k_means_pca_3$`k.means.fit.pca$cluster`)
colnames(comparison) <- c("Original","PCA")
comparison
unmatch <- subset(comparison, Original != PCA)
unmatch


#-----------------------------------
#isolate names of cities
c1 <- subset(res_k_means_5,k.means.fit$cluster==1)
c1 <- rownames(c1)
c1
#-----------------------------------
