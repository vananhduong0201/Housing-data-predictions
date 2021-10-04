rm(list=ls())

library(dplyr)
library(ggplot2)
library(corrplot)
library(ModelMetrics)

# Load the data set
housing<-read.csv('Housing_data.csv',header=TRUE,sep=',')
str(housing)

# check if there is any missing/null/NA/duplicated value in the data
any(is.na(housing))
any(is.null(housing))
duplicated(housing)
summary(housing)

# Basic ploting
plot(housing$MEDV,housing)
# Closer look to relationship of each variable with housing price
par(mfrow=c(3,3))
plot(housing$MEDV,housing$PCCR)
plot(housing$MEDV,housing$PRLZ)
plot(housing$MEDV,housing$INDUS)
plot(housing$MEDV,housing$NOX)
plot(housing$MEDV,housing$AVR)
plot(housing$MEDV,housing$AGE)
plot(housing$MEDV,housing$DIS)
plot(housing$MEDV,housing$TAX)
plot(housing$MEDV,housing$RAD)


par(mfrow=c(3,3))
hist(housing$PCCR, main="PCCR",xlab="Percentage", ylab="Observations")
hist(housing$PRLZ, main="PRLZ",xlab="Percentage", ylab="Observations")
hist(housing$INDUS, main='INDUS',xlab="Percentage",ylab="Observations")
hist(housing$NOX, main="NOX",xlab="Parts per 10 mil",ylab="Observations")
hist(housing$AVR, main="AVR",xlab="Rooms/Dwelling", ylab="Observations")
hist(housing$AGE, main = "AGE",xlab="Percentage",ylab="Observations")
hist(housing$DIS, main="DIS",xlab="Index",ylab="Observations")
hist(housing$TAX, main="TAX",xlab="$",ylab="Observations")
hist(housing$MEDV, main="MEDV",xlab="$1000",ylab="Observations")




# Checking mean, median,min & max of all variables
summary(housing)

# Checking for outliers
boxplot(housing,main="Box Plot Analysis",xlab="", ylab="value")
boxplot(housing$PCCR, housing$PRLZ,housing$AVR, housing$DIS,housing$MEDV, outpch=1,
        xlab="", ylab="value",
        names=c("PCCR","PRLZ","AVR","DIS","MEDV"),
        main="Box Plot Analysis")

# Checking correlation
cor(housing,method='pearson') # strongest cor with price: AVR
corrplot(cor(housing), "ellipse")
corrplot(cor(housing), "number")
# Make a model
model<-lm(MEDV~PCCR+PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX,housing)
summary(model)

# Improve model - remove intercept & variables with large p-value
model1<-lm(MEDV~PCCR+PRLZ+NOX+AVR+AGE+DIS+TAX+RAD+0,housing)
summary(model1)

# Fitted values analysis
fitted.values(model1)
coef(model1)

par(mfrow=c(1,1))
plot(fitted.values(model1),xlab="Observations",ylab="Fitted.values",main="Fitted Analysis",col="red",pch=16,ylim=c(-10,50))
abline(a=mean(fitted.values(model1)),b=0,col="black",lty=2)
abline(a=min(fitted.values(model1)),b=0,col="red",lty=2)
abline(a=max(fitted.values(model1)),b=0,col="red",lty=2)

# Compare original model and fitted value
plot(housing$MEDV,xlab="Observations",ylab="House prices in $1000s",col="blue",type='l', ylim=c(-10,50))
lines(fitted.values(model1), col="red")
title("Fitted values(red) vs Original data(blue)")

#Calculate Mean Square Error
predict<-predict(model,housing)
MSE<- mse(predict,housing$MEDV) #31.45

# Residual analysis
residuals(model1)
plot(density(residuals(model1)))

plot(residuals(model1),xlab="Observations",ylab="Residuals",main="Residual Analysis",col="blue",pch=16,ylim=c(-30,50))
abline(a=mean(residuals(model1)),b=0,col="black",lty=2)
abline(a=min(residuals(model1)),b=0,col="red",lty=2)
abline(a=max(residuals(model1)),b=0,col="red",lty=2)



