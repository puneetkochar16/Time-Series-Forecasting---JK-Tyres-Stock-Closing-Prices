library(forecast)
library(ggplot2)
library(fGarch)
library(zoo)
library(tseries)

setwd("D:\\Ms In Business Analytics\\Spring 2020\\Time Series Forecasting\\Final Project")
JkStock =  read.csv("JKTYRE.NS.csv")

##########size and structure of data######################
dim(JkStock)
names(JkStock)
str(JkStock)
attributes(JkStock)
head(JkStock)
tail(JkStock)


########### Summary of different columns #############
summary(JkStock)


########### Checking for Missing-Values##############
sum(is.null(JkStock))

######### converting to a time series ##########
jkPrice = ts(JkStock$Adj.Close, start = c(2010,1), frequency = 1)
tail(jkPrice)
length(jkPrice)
###### Time series Plot #######
plot(jkPrice, xlab = "Time", ylab = "Stock Prices", bty = "l")

#### seperating training and test data #######
# training data -  from year 2010 to 2018
#test data - 2019

stepsAhead <- 151
nTrain <- length(jkPrice) - stepsAhead
train.ts <- window(jkPrice, start = c(2010, 1), end = c(2010,nTrain))
tail(train.ts)
head(train.ts)
valid.ts <- window(jkPrice, start = c(2010, nTrain + 1), end = c(2010, nTrain + stepsAhead))
head(valid.ts) 
tail(valid.ts)


###### Stationarity Check ######

adf.test(train.ts,alternative="stationary")


#########  Stationarize the Series ######

# first order differencing
plot(diff(jkPrice), type="l",main="JkTyres 2010-2020",ylab="Price Differences",xlab="Days")

# Augmented Dickey-Fuller Test ADF
adf.test(diff(train.ts),alternative="stationary")


#########  Find Optimal Parameters #######

# ARIMA Models Specification
# Normal and Partial Autocorrelation Functions ACF & PACF
acf(diff(train.ts))
pacf(diff(train.ts))






model3 <- Arima(valid.ts,model=arModel)$fitted
plot(train.ts,main="ARIMA(0,1,0) using One-Step Forecast without Re-Estimation",ylab="Price",xlab="Date",ylim=c(min(jkPrice),max(jkPrice)))
lines(model3,col="green")
lines(valid.ts,lty=3)
legend("bottomright",col="green",lty=1,legend="Forecasted Price")


model3fcast = forecast(model3, h = 30)

accuracy(model3,valid.ts)

plot(forecast(model3,h=30),main="ARIMA(2,1,1) using Multi-Steps Forecast",ylab="Price",xlab="Date")
lines(valid.ts,lty=3)


arModel1 = arima(jkPrice, order = c(2,1,1))
arModel1
tsdiag(arModel1)
Box.test(arModel1$residuals, lag = 7)

arModel1fcast = forecast(arModel1, h = 30)


