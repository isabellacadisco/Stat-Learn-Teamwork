library(readxl)
library(dplyr)
library(ggplot2)
library(psych)
library(ggpubr)
library(gplots)
library(plyr)
library(car)
library(corrplot)
library(MASS)
library(caret)


#Import the data 
data <- read_excel("MC.xlsx")
colnames(data)
head(data)


#Declare factor variable and work with log GDP
data$LOCATION<-as.factor(data$LOCATION)
data$logGDP<-log(data$GDP)
head(data)
str(data)


#Does the variables differ by country?
boxplot(GDP~LOCATION, data=data) #GDP
boxplot(EDU~LOCATION, data=data) #Education
boxplot(SE~LOCATION, data=data) #Self Employment
boxplot(TAX~LOCATION, data=data) #Tax rate

plotmeans(GDP~LOCATION, data=data)
plotmeans(EDU~LOCATION, data=data)
plotmeans(SE~LOCATION, data=data)
plotmeans(TAX~LOCATION, data=data)


#Are measurements equal? F test
summary(aov(GDP ~ LOCATION, data=data))
summary(aov(EDU ~ LOCATION, data=data))
summary(aov(SE ~ LOCATION, data=data))
summary(aov(TAX ~ LOCATION, data=data))

#Descriptive statistics
describeBy(data, group = data$LOCATION)


#Split the data 
CAN<-data[data$LOCATION=="CAN",]
FRA<-data[data$LOCATION=="FRA",]
USA<-data[data$LOCATION=="USA",]
DEU<-data[data$LOCATION=="DEU",]


#Graphs per country - GDP trend
ggplot(data=CAN, aes(x=TIME, y=GDP, group=1)) +
  geom_line(color="red")+
  ggtitle("GDP of CANADA (2000-2018)")+
  geom_point()

ggplot(data=FRA, aes(x=TIME, y=GDP, group=1)) +
  geom_line(color="red")+
  ggtitle("GDP of FRANCE (2000-2018)")+
  geom_point()

ggplot(data=DEU, aes(x=TIME, y=GDP, group=1)) +
  geom_line(color="red")+
  ggtitle("GDP of GERMANY (2000-2018)")+
  geom_point()

ggplot(data=USA, aes(x=TIME, y=GDP, group=1)) +
  geom_line(color="red")+
  ggtitle("GDP of USA (2000-2018)")+
  geom_point()


#Normality tests - GDP
ggqqplot(CAN$logGDP, title = "Q-Q plot: GDP")
ggqqplot(FRA$logGDP, title = "Q-Q plot: GDP")
ggqqplot(DEU$logGDP, title = "Q-Q plot: GDP")
ggqqplot(USA$logGDP, title = "Q-Q plot: GDP")

#Normality tests - EDU
ggqqplot(CAN$EDU, title = "Q-Q plot: EDU")
ggqqplot(FRA$EDU, title = "Q-Q plot: EDU")
ggqqplot(DEU$EDU, title = "Q-Q plot: EDU")
ggqqplot(USA$EDU, title = "Q-Q plot: EDU")

#Normality tests - SE
ggqqplot(CAN$SE, title = "Q-Q plot: SE")
ggqqplot(FRA$SE, title = "Q-Q plot: SE")
ggqqplot(DEU$SE, title = "Q-Q plot: SE") #does not seem normal
ggqqplot(USA$SE, title = "Q-Q plot: SE")

#Normality tests - TAX
ggqqplot(CAN$TAX, title = "Q-Q plot: TAX")
ggqqplot(FRA$TAX, title = "Q-Q plot: TAX")
ggqqplot(DEU$TAX, title = "Q-Q plot: TAX") 
ggqqplot(USA$TAX, title = "Q-Q plot: TAX")


#Scatterplot GDP and EDU relation
plot(logGDP~EDU, col="darkgreen", pch=19, cex=1,data=CAN)
plot(logGDP~EDU, col="darkgreen", pch=19, cex=1,data=FRA)
plot(logGDP~EDU, col="darkgreen", pch=19, cex=1,data=DEU)
plot(logGDP~EDU, col="darkgreen", pch=19, cex=1,data=USA)

#Scatterplot GDP and SE relation
plot(logGDP~SE, col="darkgreen", pch=19, cex=1,data=CAN)
plot(logGDP~SE, col="darkgreen", pch=19, cex=1,data=FRA)
plot(logGDP~SE, col="darkgreen", pch=19, cex=1,data=DEU)
plot(logGDP~SE, col="darkgreen", pch=19, cex=1,data=USA)

#Scatterplot GDP and TAX relation
plot(logGDP~TAX, col="darkgreen", pch=19, cex=1,data=CAN)
plot(logGDP~TAX, col="darkgreen", pch=19, cex=1,data=FRA)
plot(logGDP~TAX, col="darkgreen", pch=19, cex=1,data=DEU)
plot(logGDP~TAX, col="darkgreen", pch=19, cex=1,data=USA)


#Correlation Matrixes
rCAN<-cor(CAN[c('GDP','EDU','SE','TAX')])
round(rCAN,2)
corrplot(rCAN, type = "upper", tl.col = "black", tl.srt = 45)

rFRA<-cor(FRA[c('GDP','EDU','SE','TAX')])
round(rFRA,2)
corrplot(rFRA, type = "upper", tl.col = "black", tl.srt = 45)

rDEU<-cor(DEU[c('GDP','EDU','SE','TAX')])
round(rDEU,2)
corrplot(rDEU, type = "upper", tl.col = "black", tl.srt = 45)

rUSA<-cor(USA[c('GDP','EDU','SE','TAX')])
round(rUSA,2)
corrplot(rUSA, type = "upper", tl.col = "black", tl.srt = 45)

#Correlation Matrix
scatterplotMatrix(data, regLine = TRUE)



#Drop TIME and LOCATION from subsets
CAN<-CAN[,3:7]
FRA<-FRA[,3:7]
DEU<-DEU[,3:7]
USA<-USA[,3:7]


#Split data into training and test sets
set.seed(1)
train_obs_CAN<-CAN$logGDP %>% createDataPartition(p = 0.8, list = FALSE)
train_CAN <- CAN[train_obs_CAN, ]
test_CAN <- CAN[-train_obs_CAN, ]

train_obs_FRA<-FRA$logGDP %>% createDataPartition(p = 0.8, list = FALSE)
train_FRA <- FRA[train_obs_FRA, ]
test_FRA <- FRA[-train_obs_FRA, ]

train_obs_DEU<-DEU$logGDP %>% createDataPartition(p = 0.8, list = FALSE)
train_DEU <- DEU[train_obs_DEU, ]
test_DEU <- DEU[-train_obs_DEU, ]

train_obs_USA<-USA$logGDP %>% createDataPartition(p = 0.8, list = FALSE)
train_USA <- USA[train_obs_USA, ]
test_USA <- USA[-train_obs_USA, ]


#Fit models
#1) logGDP~EDU This is the reference model
#2) logGDP~EDU+SE+TAX Full model
#3) logGDP~EDU+SE^(2)+TAX*EDU
#4) Piecewise selection


#CANADA
CAN1 <-lm(logGDP~EDU, data = train_CAN)
summary(CAN1)
confint(CAN1)
shapiro.test(CAN1$residuals)
ggqqplot(CAN1$residuals)

CAN2 <-lm(logGDP~EDU+SE+TAX, data=train_CAN)
summary(CAN2)
confint(CAN2)
shapiro.test(CAN2$residuals)
ggqqplot(CAN2$residuals)
vif(CAN2)
sqrt(vif(CAN2)) > 2

CAN3 <-lm(logGDP~EDU+poly(SE,2)+TAX:EDU, data=train_CAN)
summary(CAN3)
confint(CAN3)
shapiro.test(CAN3$residuals)
ggqqplot(CAN3$residuals)
vif(CAN3, type='predictor')
sqrt(vif(CAN3)) > 2

CAN4 <-stepAIC(CAN2, direction = "both",trace = FALSE, k=2)
summary(CAN4)
confint(CAN4)
shapiro.test(CAN4$residuals)
ggqqplot(CAN4$residuals)
vif(CAN4)
sqrt(vif(CAN4)) > 2

#Test models
pred_CAN1 <- CAN1 %>% predict(test_CAN)
pCAN1<-data.frame(R_squared = R2(pred_CAN1, test_CAN$logGDP),
           RMSE = RMSE(pred_CAN1, test_CAN$logGDP),
           MAE = MAE(pred_CAN1, test_CAN$logGDP))

pred_CAN2 <- CAN2 %>% predict(test_CAN)
pCAN2<-data.frame(R_squared = R2(pred_CAN2, test_CAN$logGDP),
           RMSE = RMSE(pred_CAN2, test_CAN$logGDP),
           MAE = MAE(pred_CAN2, test_CAN$logGDP))

pred_CAN3 <- CAN3 %>% predict(test_CAN)
pCAN3<-data.frame(R_squared = R2(pred_CAN3, test_CAN$logGDP),
           RMSE = RMSE(pred_CAN3, test_CAN$logGDP),
           MAE = MAE(pred_CAN3, test_CAN$logGDP))

pred_CAN4 <- CAN4 %>% predict(test_CAN)
pCAN4<-data.frame(R_squared = R2(pred_CAN4, test_CAN$logGDP),
           RMSE = RMSE(pred_CAN4, test_CAN$logGDP),
           MAE = MAE(pred_CAN4, test_CAN$logGDP))

results_CAN <-c("Model 1"=pCAN1$RMSE,
                "Model 2"=pCAN2$RMSE,
                "Model 3"=pCAN3$RMSE,
                "Model 4"=pCAN4$RMSE)

round(results_CAN,3)
min(results_CAN)

#Best model is CAN4: lowest RMSE



#FRANCE
FRA1 <-lm(logGDP~EDU, data = train_FRA)
summary(FRA1)
confint(FRA1)
shapiro.test(FRA1$residuals)
ggqqplot(FRA1$residuals)

FRA2 <-lm(logGDP~EDU+SE+TAX, data=train_FRA)
summary(FRA2)
confint(FRA2)
shapiro.test(FRA2$residuals)
ggqqplot(FRA2$residuals)
vif(FRA2)
sqrt(vif(FRA2)) > 2

FRA3 <-lm(logGDP~EDU+poly(SE,2)+TAX:EDU, data=train_FRA)
summary(FRA3)
confint(FRA3)
shapiro.test(FRA3$residuals)
ggqqplot(FRA3$residuals)
vif(FRA3, type='predictor')
sqrt(vif(FRA3)) > 2

FRA4 <-stepAIC(FRA2, direction = "both",trace = FALSE, k=2)
summary(FRA4)
confint(FRA4)
shapiro.test(FRA4$residuals)
ggqqplot(FRA4$residuals)
vif(FRA4)
sqrt(vif(FRA4)) > 2

#Test models
pred_FRA1 <- FRA1 %>% predict(test_FRA)
pFRA1<-data.frame(R_squared = R2(pred_FRA1, test_FRA$logGDP),
                  RMSE = RMSE(pred_FRA1, test_FRA$logGDP),
                  MAE = MAE(pred_FRA1, test_FRA$logGDP))

pred_FRA2 <- FRA2 %>% predict(test_FRA)
pFRA2<-data.frame(R_squared = R2(pred_FRA2, test_FRA$logGDP),
                  RMSE = RMSE(pred_FRA2, test_FRA$logGDP),
                  MAE = MAE(pred_FRA2, test_FRA$logGDP))

pred_FRA3 <- FRA3 %>% predict(test_FRA)
pFRA3<-data.frame(R_squared = R2(pred_FRA3, test_FRA$logGDP),
                  RMSE = RMSE(pred_FRA3, test_FRA$logGDP),
                  MAE = MAE(pred_FRA3, test_FRA$logGDP))

pred_FRA4 <- FRA4 %>% predict(test_FRA)
pFRA4<-data.frame(R_squared = R2(pred_FRA4, test_FRA$logGDP),
                  RMSE = RMSE(pred_FRA4, test_FRA$logGDP),
                  MAE = MAE(pred_FRA4, test_FRA$logGDP))

results_FRA <-c("Model 1"=pFRA1$RMSE,
                "Model 2"=pFRA2$RMSE,
                "Model 3"=pFRA3$RMSE,
                "Model 4"=pFRA4$RMSE)

round(results_FRA,4)
min(results_FRA)

#Best model is FRA3: lowest RMSE


#GERMANY
DEU1 <-lm(logGDP~EDU, data = train_DEU)
summary(DEU1)
confint(DEU1)
shapiro.test(DEU1$residuals)
ggqqplot(DEU1$residuals)

DEU2 <-lm(logGDP~EDU+SE+TAX, data=train_DEU)
summary(DEU2)
confint(DEU2)
shapiro.test(DEU2$residuals)
ggqqplot(DEU2$residuals)
vif(DEU2)
sqrt(vif(DEU2)) > 2

DEU3 <-lm(logGDP~EDU+poly(SE,2)+TAX:EDU, data=train_DEU)
summary(DEU3)
confint(DEU3)
shapiro.test(DEU3$residuals)
ggqqplot(DEU3$residuals)
vif(DEU3, type='predictor')
sqrt(vif(DEU3)) > 2

DEU4 <-stepAIC(DEU2, direction = "both",trace = FALSE, k=2)
summary(DEU4)
confint(DEU4)
shapiro.test(DEU4$residuals)
ggqqplot(DEU4$residuals)
vif(DEU4)
sqrt(vif(DEU4)) > 2

#Test models
pred_DEU1 <- DEU1 %>% predict(test_DEU)
pDEU1<-data.frame(R_squared = R2(pred_DEU1, test_DEU$logGDP),
                  RMSE = RMSE(pred_DEU1, test_DEU$logGDP),
                  MAE = MAE(pred_DEU1, test_DEU$logGDP))

pred_DEU2 <- DEU2 %>% predict(test_DEU)
pDEU2<-data.frame(R_squared = R2(pred_DEU2, test_DEU$logGDP),
                  RMSE = RMSE(pred_DEU2, test_DEU$logGDP),
                  MAE = MAE(pred_DEU2, test_DEU$logGDP))

pred_DEU3 <- DEU3 %>% predict(test_DEU)
pDEU3<-data.frame(R_squared = R2(pred_DEU3, test_DEU$logGDP),
                  RMSE = RMSE(pred_DEU3, test_DEU$logGDP),
                  MAE = MAE(pred_DEU3, test_DEU$logGDP))

pred_DEU4 <- DEU4 %>% predict(test_DEU)
pDEU4<-data.frame(R_squared = R2(pred_DEU4, test_DEU$logGDP),
                  RMSE = RMSE(pred_DEU4, test_DEU$logGDP),
                  MAE = MAE(pred_DEU4, test_DEU$logGDP))

results_DEU <-c("Model 1"=pDEU1$RMSE,
                "Model 2"=pDEU2$RMSE,
                "Model 3"=pDEU3$RMSE,
                "Model 4"=pDEU4$RMSE)

round(results_DEU,2)
min(results_DEU)

#Best model is DEU4: lowest RMSE



#USA
USA1 <-lm(logGDP~EDU, data = train_USA)
summary(USA1)
confint(USA1)
shapiro.test(USA1$residuals)
ggqqplot(USA1$residuals)

USA2 <-lm(logGDP~EDU+SE+TAX, data=train_USA)
summary(USA2)
confint(USA2)
shapiro.test(USA2$residuals)
ggqqplot(USA2$residuals)
vif(USA2)
sqrt(vif(USA2)) > 2

USA3 <-lm(logGDP~EDU+poly(SE,2)+TAX:EDU, data=train_USA)
summary(USA3)
confint(USA3)
shapiro.test(USA3$residuals)
ggqqplot(USA3$residuals)
vif(USA3, type='predictor')
sqrt(vif(USA3)) > 2

USA4 <-stepAIC(USA2, direction = "both",trace = FALSE, k=2)
summary(USA4)
confint(USA4)
shapiro.test(USA4$residuals)
ggqqplot(USA4$residuals)
vif(USA4)
sqrt(vif(USA4)) > 2

#Test models
pred_USA1 <- USA1 %>% predict(test_USA)
pUSA1<-data.frame(R_squared = R2(pred_USA1, test_USA$logGDP),
                  RMSE = RMSE(pred_USA1, test_USA$logGDP),
                  MAE = MAE(pred_USA1, test_USA$logGDP))

pred_USA2 <- USA2 %>% predict(test_USA)
pUSA2<-data.frame(R_squared = R2(pred_USA2, test_USA$logGDP),
                  RMSE = RMSE(pred_USA2, test_USA$logGDP),
                  MAE = MAE(pred_USA2, test_USA$logGDP))

pred_USA3 <- USA3 %>% predict(test_USA)
pUSA3<-data.frame(R_squared = R2(pred_USA3, test_USA$logGDP),
                  RMSE = RMSE(pred_USA3, test_USA$logGDP),
                  MAE = MAE(pred_USA3, test_USA$logGDP))

pred_USA4 <- USA4 %>% predict(test_USA)
pUSA4<-data.frame(R_squared = R2(pred_USA4, test_USA$logGDP),
                  RMSE = RMSE(pred_USA4, test_USA$logGDP),
                  MAE = MAE(pred_USA4, test_USA$logGDP))

results_USA <-c("Model 1"=pUSA1$RMSE,
                "Model 2"=pUSA2$RMSE,
                "Model 3"=pUSA3$RMSE,
                "Model 4"=pUSA4$RMSE)

round(results_USA,2)
min(results_USA)

#Best model is USA1: lowest RMSE