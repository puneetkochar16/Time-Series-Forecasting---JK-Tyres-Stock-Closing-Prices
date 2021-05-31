library(forecast)
library(ggplot2)
library(fGarch)
library(zoo)
library(caret)
library(nnet)
library(tseries)
setwd("D:\\Ms In Business Analytics\\Spring 2020\\Time Series Forecasting\\Final Project")
JkStock =  read.csv("JKTYRE.NS.csv")


######### converting to a time series ##########
jkPrice = ts(JkStock$Adj.Close, start = c(2010,1), frequency = 1)
tail(jkPrice)
length(jkPrice)
###### Time series Plot #######
plot(jkPrice, xlab = "Time", ylab = "Stock Prices", bty = "l")

#### seperating training and test data #######
# training data -  from year 2010 to 2018
#test data - 2019

stepsAhead <- 242
nTrain <- 2461 - stepsAhead
train.ts <- window(jkPrice, start = c(2010, 1), end = c(2010,nTrain))
tail(train.ts)
head(train.ts)
valid.ts <- window(jkPrice, start = c(2010, nTrain + 1), end = c(2010, nTrain + stepsAhead))
head(valid.ts) 
tail(valid.ts)



###### neural network ####

set.seed(201)
jkPrice.nnetar = nnetar(train.ts, repeats = 20, p=11, P = 1, size =7)
summary(jkPrice.nnetar$model[[1]])#weights first train
summary(jkPrice.nnetar$model[[2]])#weights second train
jkPrice.nnetar.pred = forecast(jkPrice.nnetar,h=stepsAhead)
accuracy(jkPrice.nnetar.pred,valid.ts)
par(mar=c(1,1,1,1))
plot(train.ts,lty = 1)
lines(jkPrice.nnetar.pred$fitted,lwd = 2, col = "blue")
lines(jkPrice.nnetar.pred$mean,lwd = 2, col = "blue",lty =2)
lines(valid.ts)


