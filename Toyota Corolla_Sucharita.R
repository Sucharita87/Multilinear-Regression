#Consider only the below columns and prepare a prediction model for predicting Price.
#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

library(readr)
library(psych)
library(dummies)
library(caret)
library(corpcor)
library(car)
cprice<-read.csv("F://ExcelR//Assignment//Multiple linear regression//ToyotaCorolla.csv")
View(cprice)
corolla<-cprice[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(corolla)
summary(corolla)
str(corolla)
#data standardisation
corollastd<-preProcess(corolla, method = c("center", "scale"))
corolla1<-predict(corollastd,corolla)
summary(corolla1)

# checking corelation
pairs(corolla1)
plot(corolla1)
cor(corolla1) 
cor2pcor(cor(corolla1))# no multicollinearity problem

#model building
model1<-lm(Price~., data=corolla1)
summary(model1) # Multiple R square = 0.86, significant variables are age, km, hp, gears, quarterly_tax, wt

model2<-lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight, data = corolla1)
summary(model2) # Multiple R square = 0.86

# consider model2, as it has the highest R square value
pairs.panels(corolla1)
influence.measures(model2)
influencePlot(model2) # ifluencial points = 222,602, 961
influenceIndexPlot(model2)

# removing influencial datapoints 
model_final<-lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight, data = corolla1[-c(222,602,961)])
summary(model_final)# Multiple R square = 0.86
vif(model_final)
avPlots(model_final) 
plot(model_final)
qqPlot(model_final)

