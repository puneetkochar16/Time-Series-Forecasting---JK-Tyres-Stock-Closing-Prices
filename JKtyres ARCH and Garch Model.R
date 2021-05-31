library(forecast)
library(ggplot2)
library(fGarch)
library(zoo)
library(tseries)

setwd("D:\\Ms In Business Analytics\\Spring 2020\\Time Series Forecasting\\Final Project")
JkStock =  read.csv("JKTYRE.NS.csv")

jkprices = JkStock$Adj.Close[length(JkStock$Adj.Close):1]


jkreturns=diff(jkprices)/ jkprices[-length(jkprices)] # returns
jklogreturns = log(jkreturns + 1)

plot.ts(jklogreturns)
acf(jklogreturns)
pacf(jklogreturns)

logarma = auto.arima(jklogreturns, stepwise=FALSE,approx=FALSE)
logarma
tsdiag(logarma)
checkresiduals(logarma)

acf(logarma$residuals)
Box.test(logarma$residuals, type = "Ljung")
Box.test(logarma$residuals^2, type = "Ljung")


ghmodel=garchFit(~garch(1,1),data=jklogreturns,trace=F)
summary(ghmodel)

sresi=residuals(ghmodel,standardize=T) # Obtain standardized residuals
sigma.t=volatility(ghmodel) 

Box.test(sresi,10,type='Ljung')
Box.test(sresi^2,10,type='Ljung')

qqnorm(sresi)
qqline(sresi)
