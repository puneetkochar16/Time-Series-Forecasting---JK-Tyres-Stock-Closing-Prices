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



#### Plot of training and test data #######
plot(jkPrice,main="JK Tyres 2010-2020",ylab="Price",xlab="Days")
lines(train.ts,col="blue")
lines(valid.ts,col="green")
legend("bottomright",col=c("blue","green"),lty=1,legend=c("Training","Testing"))


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


########## ARIMA Model ######

m1<-auto.arima((train.ts))
m1
tsdisplay(m1$residuals) #### Residuals diagnostics
Box.test(m1$residuals)




arimafcast = forecast(m1, h = 151)

plot(forecast(m1,h=151),main="ARIMA(0,1,0) using Multi-Steps Forecast",ylab="Price",xlab="Date")
lines(valid.ts,lty=3)

accuracy(arimafcast,valid.ts)



##### Arima model 2 #######
m2 = arima(train.ts, order = c(0,1,0))
m2
tsdisplay(m2$residuals, lag.max = 40)
Box.test(m2$residuals)

m2fcast = forecast(m2, h = 151)

plot(forecast(m2,h=151),main="ARIMA(0,1,1) using Multi-Steps Forecast",ylab="Price",xlab="Date")
lines(valid.ts,lty=3)

accuracy(m2fcast, valid.ts)



##### arima model 3 #####
m3 = arima(train.ts, order = c(1,1,1))
m3
tsdisplay(m3$residuals)
Box.test(m3$residuals)


m3fcast = forecast(m3, h = 151)

plot(forecast(m3,h=151),main="ARIMA(1,1,1) using Multi-Steps Forecast",ylab="Price",xlab="Date")
lines(valid.ts,lty=3)

accuracy(m3fcast, valid.ts)




