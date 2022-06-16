library(pheatmap)
library(haven)
library(car)

dataset <- read.csv("C:/Users/isabe/OneDrive - Università degli Studi di Milano/3rd trimester/statistical learning/group project/QDV2021-main/QDV2021-main/qualita_vita_2021.csv")
str(dataset)

data <- dataset
n <- nrow(data)
p <- ncol(data)


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


str(data)

# correlation of the components w/ the original features

corr_comp = matrix(, nrow = 90, ncol = 90)
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


# Scree Diagram:
plot(autoval_cor, type="b", main="Scree Diagram", xlab="Components", ylab="Eigenvalue")
abline(h=1, lwd=3, col="red")



#USIAMO RIS PCA PER KMEANS






