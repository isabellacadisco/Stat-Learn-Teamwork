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

set.seed(123)
k.means.fit <- kmeans(data_std, 3) # k = 3
str(k.means.fit)
res_k_means <- as.data.frame(k.means.fit$cluster)
clustered <- as.data.frame(cbind(data_std,k.means.fit$cluster))
write.csv(cbind(data_std,k.means.fit$cluster),"clustered.csv")

#isolate names of cities
c1 <- subset(res_k_means,k.means.fit$cluster==1)
c1 <- rownames(c1)
c1
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

#scree plot
plot(autoval_cor, type="b", main="Scree Diagram", xlab="Components", ylab="Eigenvalue")
abline(h=1, lwd=3, col="red")

# projection of the original dataset

data_pca = as.matrix(data[,2:91]) %*% autovec_cor[,1:18]
rownames(data_pca) = data[,1]
colnames(data_pca) = c(1:18)
data_pca

set.seed(123)
k.means.fit.pca <- kmeans(data_pca, 3) # k = 5
str(k.means.fit.pca)
res_k_means_pca <- as.data.frame(k.means.fit.pca$cluster)
comparison = cbind(res_k_means, res_k_means_pca$`k.means.fit.pca$cluster`)
colnames(comparison) <- c("Original","PCA")
comparison
unmatch <- subset(comparison, Original != PCA)



#geocoding (tableau visualization part)
library(httr)
library(jsonlite)

#access token to mapbox
coordinates <- c()
map_key <- ".json?country=it&limit=1&access_token=pk.eyJ1Ijoid2lsbGlhbS1iaW9uZGkiLCJhIjoiY2w0aWcwZ3lnMDFlbDNqbWh2azRzbm9icyJ9.OT-ffTY0Ind_kCcItLDUZw"
for (prov in row.names(comparison)) {
#clean the strings because mapbox api doesn't accept whitespaces and special characters in the query
  if(grepl("-",prov)){
    prov <- strsplit(prov,"-")[1]
    prov <- prov[[1]][1]
  }
  place <- tolower(gsub(" ","%20",prov))
#preparing http request 
  url <- paste("https://api.mapbox.com/geocoding/v5/mapbox.places/",place,map_key,sep="")
  req <- GET(url)
#get response data and extract coordinates
  geo_data <- fromJSON(rawToChar(req$content))
  geo_data <- geo_data[["features"]][["geometry"]][["coordinates"]]
  coordinates <- c(geo_data[1])
}
longitudes <- c()
latitudes <- c()
for (coor in coordinates){
  longitudes <-c( longitudes,coor[1])
  latitudes <- c(latitudes,coor[2])
}
  
clustered <- cbind(clustered, latitudes,longitudes)
write.csv(clustered,"clustered.csv")

