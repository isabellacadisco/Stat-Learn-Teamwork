
library(haven)
library(car)


dataset <- read.csv("C:/Users/isabe/OneDrive - Università degli Studi di Milano/3rd trimester/statistical learning/group project/QDV2021-main/QDV2021-main/qualita_vita_2021.csv")

#### Analysis for italian province ####
rownames(dataset)<-dataset$DENOMINAZIONE.CORRENTE
dataset<-dataset[,-1]

head(dataset)

summary(dataset)

summary(scale(dataset)
      
        

ds<-dist(scale(dataset))
## function for agglomeration program 
agglo <- function(hc){
  data.frame(row.names=paste0("Cluster",seq_along(hc$height)),
             height=hc$height,
             components=ifelse(hc$merge<0, hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge)),
             stringsAsFactors=FALSE) }


opar <- par(mfrow = c(1, 1))                  
### average linkage
h1<-hclust(ds, method="average")
agglo(h1)


plot(h1, main="average linkage")

### complete linkage
h2<-hclust(ds, method="complete")
agglo(h2)        
        
plot(h2, main="complete linkage")


### legame ward
h3<-hclust(ds, method="ward.D2")
agglo(h3)

plot(h3, main="Ward linkage")


average <- cutree(h1, k=5)
complete<- cutree(h2, k=5)
ward<- cutree(h3, k=5)
table(average,complete)


table(average,ward)

table(complete,ward)

### dendrograms
plot(h1, main="Complete linkage")
rect.hclust(h2, 5)
h2cluster <- cutree(h2, k=5)
h2cluster

plot(dataset, col=h2cluster, main="complete likage")

table(h2cluster)

## explore solution
medie<-aggregate(dataset, list(h2cluster), mean)
medie 

## standardized variable
mediez<-aggregate(scale(dataset), list(h2cluster), mean)
mediez

##### calculate R^2
mydata<-dataset
mydata$group<-h2cluster
R2 <- rep(NA, (ncol(mydata)-1))
for(i in 1:(ncol(mydata)-1)) 
  R2[i] <- anova(aov(mydata[,i] ~ mydata[,ncol(mydata)]))[1,2]/(anova(aov(mydata[,i] ~ mydata[,ncol(mydata)]))[1,2]+anova(aov(mydata[,i] ~ mydata[,ncol(mydata)]))[2,2])

R2

mydata<-mydata[,-ncol(mydata)]
col<-colnames(mydata)
finali<-cbind(col,R2)
finali

##### K means 
# How many clusters??
wss <- (nrow(mydata))*sum(apply(mydata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Numero of cluster", ylab="Within Deviance")

# Explore K mean solution 
fit <- kmeans(mydata, 4) # 4 cluster solution

aggregate(mydata,by=list(fit$cluster),FUN=mean)

# add cluster to data
mydata <- data.frame(mydata, fit$cluster)

table(h2cluster,fit$cluster)







#### Exercice: apply PCA to the same dataset!


#######PCA###### 

n <- nrow(dataset)
p <- ncol(dataset)


# descriptives:
medie <- colMeans(dataset)
scarto <- apply(dataset, 2, sd)
descrittive<-round(cbind(medie, scarto),2)
descrittive

## PCA from correlation:
rho <- cor(dataset)

eigen(rho)

autoval <- eigen(rho)$values
autovec <- eigen(rho)$vectors

# Select components
pvarsp = autoval/p
pvarspcum = cumsum(pvarsp)
pvarsp


# Scree Diagram:
plot(autoval, type="b", main="Scree Diagram", xlab="Components", ylab="Eigenvalue")
abline(h=1, lwd=3, col="red")

### Interpret the components:
eigen(rho)$vectors[,1:2]


#  Component matrix, obtained by multiplying the eigenvector by the root of the respective eigenvalue (we change the sign only for interpretative reasons) 
comp<-round(cbind(-eigen(rho)$vectors[,1]*sqrt(autoval[1]),-eigen(rho)$vectors[,2]*sqrt(autoval[2])),3)
rownames(comp)<-row.names(descrittive)
colnames(comp)<-c("Comp1","Comp2")
comp

# the sum of the squares of the values of each row of the component matrix is the respective commonality
comunality<-comp[,1]^2+comp[,2]^2
comp<-cbind(comp,comunality)
comp


# scores:
dataset.scale <- scale(dataset, T, T)
punteggi <- dataset.scale%*%autovec[,1:2]
# standardized scores
punteggiz<-round(cbind(-punteggi[,1]/sqrt(autoval[1]),-punteggi[,2]/sqrt(autoval[2])),2)
plot(punteggiz, main="Score plot",
     xlab="comp1",ylab="comp2")
text(punteggiz, rownames(dataset))
abline(v=0,h=0,col="red")


# loadinngs
plot(comp[,1:2], main="Loadings plot",
     xlab="comp1",ylab="comp2", xlim=range(-1,1))
text(comp, rownames(comp))
abline(v=0,h=0,col="red")

#### princomp :
acp<-princomp(dataset, cor=T)
summary(princomp(dataset, cor=T))


# number of components:
screeplot(princomp(dataset, cor=T))
# plots:
plot(princomp(dataset, cor=T)$scores)
text(princomp(dataset, cor=T)$scores, rownames(dataset))
abline(h=0, v=0)

## biplot 
biplot(acp)
