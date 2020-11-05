#Prepare a prediction model for profit of 50_startups data.Do transformations for getting better predictions of profit and
#Make a table containing R^2 value for each prepared model.
#R&D Spend -- Research and devolop spend in the past few years 
#Administration-- spend on administration in the past few years
#Marketing Spend -- spend on Marketing in the past few years 
#State -- states from which data is collected 
#Profit -- profit of each state in the past few years
# Ho= Variables not related to profit
# Ha= Variables related and impacts profit

library(readr)
library(dummies)
library(corpcor)
library(psych)
library(car)
startup<-read.csv("F://ExcelR//Assignment//Multiple linear regression//50_Startups.csv")
View(startup)
attach(startup)
levels(State)
dummy.data.frame(data=startup, variable.names(State)) # create dummy dataframe by replacing categorical vaues with numeric
d<-dummy.data.frame(startup) # new df with all continuous data
View(d)
summary(d)
pairs(startup)
plot(startup)
cor(d)
attach(d)
RDS<- R.D.Spend
ADM<-Administration
MS<-Marketing.Spend
cor2pcor(cor(d)) # values near to 1, depict variables which are interrelated, depicting collinearity problem

mreg<-lm(Profit~RDS+ADM+MS) #linear model based on all parameters
summary(mreg) # R square value = 0.9507, low p value for RDS, 
#signifies that change in RDS impacts Profit

mreg1<-lm(Profit~RDS)# linear model considering only R and D spend
summary(mreg1)# R square value = 0.9465, low p value, RDS impacts Profit

mreg2<-lm(Profit~ADM) # linear model considering only Administration
summary(mreg2) # R square value = 0.04029, very low, so cannot be considered for model building

mreg3<-lm(Profit~MS) # linear model considering only Marketing spend
summary(mreg3) # R square value = 0.5592, low p value, signifies that variable MS and profit are related,
# but as Rsquare value is less, so cannot be considered for model building

mreg4<-lm(Profit~RDS+ADM) # linear model considering R and D spend and Administration
summary(mreg4) # R square value = 0.9478, low p value for RDS but p-value for ADM= 0.289, 
#as its >0.05 so null hypothesis is accepted that ADM is not related to profit, but RDS is related

mreg5<-lm(Profit~ADM+MS) # linear model considering Administration and Marketing spend
summary(mreg5) # R square value =0.6097, low p value for MS, 
#but as R square value is very less, so cannot be considered for model building

mreg6<-lm(Profit~RDS+MS) # linear model considering R and D spend and Marketing spend
summary(mreg6) # R square value =0.9505, low p value for RDS but high p value for MS.  

pairs.panels(startup)
influence.measures(mreg)
influenceIndexPlot(mreg6)
influencePlot(mreg6)
influencePlot(mreg)

mreg_all<-lm(Profit~RDS+ADM+MS, data = d[-c(46,47,49,50)]) # delete, 46th, 47th, 49th and 50th observation
summary(mreg_all) # R square value =0.9507, consider this as a better model

mreg_rds_ms<-lm(Profit~RDS+MS, data =d[-c(20, 46, 47, 50)])
summary(mreg_rds_ms) # R square value =0.9505

vif(mreg_all)
avPlots(mreg_all) # ADM and profit are not much related, so we can exclude ADM

final<-lm(Profit~RDS+MS, data =d[-c(20,46, 47, 50)])
summary(final) # R square value = 0.9505

plot(final)
qqPlot(final)


