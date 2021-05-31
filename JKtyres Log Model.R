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

# Select the relevant close price series
jkPrice = JkStock[,4]

logstock = log(jkPrice[1:2219])

### Acf And PACF
acf(logstock, lag.max = 20)
pacf(logstock, lag.max = 20)
difflnstock = diff(logstock,1)
adf.test(logstock)
adf.test(difflnstock)


pricearima =  ts(logstock, start = c(2010,1), frequency = 12)
fitlnstock = auto.arima(pricearima)
tsdiag(fitlnstock)
Box.test(fitlnstock$residuals, lag = 20)


plot(pricearima, type = "l")
exp(logstock)

#### forecasted values for arima
forecastedvalues_ln = forecast(fitlnstock, h = 242)
forecastedvalues_ln
plot(forecastedvalues_ln)

forecastedvaluesectracted = as.numeric(forecastedvalues_ln$mean)
finalforecastedvalues = exp(forecastedvaluesectracted)
finalforecastedvalues


#percentage error
df = data.frame(jkPrice[2220:2461], finalforecastedvalues)
col_headings =  c("Actual Price", "Forecasted Price")
names(df)  = col_headings
attach(df)
pe = ((df$`Actual Price`- df$`Forecasted Price`)/ (df$`Actual Price`))
pe
mean(pe)
df
 