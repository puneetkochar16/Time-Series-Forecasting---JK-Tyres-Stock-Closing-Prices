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
jkPrice = ts(JkStock$Adj.Close, start = c(2010,1), frequency = 250)
tail(jkPrice)
length(jkPrice)
###### Time series Plot #######
plot(jkPrice, xlab = "Time", ylab = "Stock Prices", bty = "l")

#### seperating training and test data #######
# training data -  from year 2010 to 2018
#test data - 2019

plot(decompose(jkPrice))

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



##### Holt- Winters #####

hwModel = HoltWinters(train.ts)
hwfcast2 = forecast(hwModel, h = 151 )
plot(hwfcast2) 
lines(valid.ts)
Acf(hwfcast2$residuals, lag.max=7)                     
Box.test(hwfcast2$residuals, lag=7, type="Ljung-Box")

accuracy(hwfcast2, valid.ts)

hwModel = HoltWinters(train.ts)
hwfcast2 = forecast(hwModel, h = 181 )
plot(hwfcast2)
lines(valid.ts)

accuracy(hwfcast2, valid.ts)
