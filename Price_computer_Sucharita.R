#Predict Price of the computer
library(readr)
library(corpcor)
library(psych)
library(dummies)
library(dplyr)
library(car)
com_data<-read.csv("F://ExcelR//Assignment//Multiple linear regression//Computer_data.csv")
View(com_data)
com_new= select(com_data, -1)
View(com_new)
com_new_dummy<-dummy.data.frame(com_new)
View(com_new_dummy)
summary(com_new_dummy)
pairs(com_new_dummy)
plot(com_new_dummy)
cor(com_new_dummy)
cor2pcor(cor(com_new_dummy))
attach(com_new)

mreg1<-lm(price~speed+hd+ram+screen+ads+trend) # linear model with all variables
summary(mreg1) # R square value = 0.7123, all p values are low which depict variables are related to Price

mreg2<-lm(price~speed)
summary(mreg2) # R square value = 0.090, very low and cannot be considered for model building

mreg3<-lm(price~speed+hd)
summary(mreg3) # R square value = 0.2081, very low and cannot be considered for model building

mreg4<-lm(price~hd+ram+screen)
summary(mreg4) # R square value = 0.4279, very low and cannot be considered for model building

mreg5<-lm(price~speed+hd+ram+screen)
summary(mreg5) # R square value = 0.4589, very low and cannot be considered for model building

mreg6<-lm(price~ads+trend)
summary(mreg6) # R square value = 0.04, very low and cannot be considered for model building

mreg7 <-lm(price~hd+ram)
summary(mreg7) # R square value = 0.39, very low and cannot be considered for model building

# consider mreg1 model, as it has the highest R square value
pairs.panels(com_new)
influence.measures(mreg1)
influencePlot(mreg1)
influenceIndexPlot(mreg1)

mreg1_new<-lm(price~., data= com_new[-c(1441, 1701)])
summary(mreg1_new) # R square value = 0.7756, higher than previous values

vif(mreg1_new)
avPlots(mreg1_new) # variables that can be discarded are "cd" and "multi"

mreg1_new1<-lm(price~speed+hd+ram+screen+ads+trend+premium , data= com_new[-c(1441, 1701, 3784, 4478)])
summary(mreg1_new1)# R square value = 0.7681

vif(mreg1_new1)
avPlots(mreg1_new1) # remove "ads"

plot(mreg1_new1)
qqPlot(mreg1_new1)

