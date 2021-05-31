library(forecast)
library(ggplot2)
library(fGarch)
library(zoo)

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
nTrain <- lengh - stepsAhead
train.ts <- window(jkPrice, start = c(2010, 1), end = c(2010,nTrain))
tail(train.ts)
head(train.ts)
valid.ts <- window(jkPrice, start = c(2010, nTrain + 1), end = c(2010, nTrain + stepsAhead))
head(valid.ts) 
tail(valid.ts)



####### Simple Exponential Smoothing #######

ses.Jk.train.dif <- ses(diff(train.ts), alpha = .05, h = 242)
autoplot(ses.Jk.train.dif)
accuracy(ses.Jk.train.dif, diff(valid.ts))
