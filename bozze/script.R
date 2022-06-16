dataset <- read.csv("C:/Users/isabe/OneDrive - Università degli Studi di Milano/3rd trimester/statistical learning/group project/QDV2021-main/QDV2021-main/qualita_vita_2021.csv")
str(dataset)

data <- dataset[,-1]

n <- nrow(data)
p <- ncol(data)
n
p


#DESCRIPTIVE
#mean and std
medie <- colMeans(data)
scarto <- apply(data, 2, sd)
medie
scarto

#PCA from covariance matrix
sigma <- cov(data)
eigen(sigma)

autoval <- eigen(sigma)$values
autovec <- eigen(sigma)$vectors
autovec


#PCA from correlation matrix
rho <- cor(data)
eigen(rho)

autoval <- eigen(rho)$values
autovec <- eigen(rho)$vectors
autovec

comp<-round(cbind(-eigen(rho)$vectors[,1]*sqrt(autoval[1]),-eigen(rho)$vectors[,2]*sqrt(autoval[2])),3)
colnames(comp)<-c("Comp1","Comp2")
comp

pvarsp = autoval/p
pvarspcum = cumsum(pvarsp)
pvarsp

#--------------------------

M <- colMeans(data)
sigma <- apply(data, 2, sd)
descriptive<-round(cbind(M, sigma),2)
descriptive


rho <- cor(data)
round(rho,3)

eigen(rho)


autoval <- eigen(rho)$values
autovec <- eigen(rho)$vectors


pvarsp = autoval/p
pvarspcum = cumsum(pvarsp)
tab<-round(cbind(autoval,pvarsp*100,pvarspcum*100),3)
colnames(tab)<-c("eigenvelues", "% variance","% cum variance")
tab


plot(autoval, type="b", main="Scree Diagram", xlab="Number of Component", ylab="Eigenvalues")
abline(h=1, lwd=3, col="red")


#PRINCOMP

acp<-princomp(data, cor=T)
summary(princomp(data, cor=T))

screeplot(princomp(data, cor=T))

row.names(data)<-dataset$DENOMINAZIONE.CORRENTE
row.names(data)


plot(princomp(data, cor=T)$scores)
text(princomp(data, cor=T)$scores, rownames(data))
abline(h=0, v=0)
