#Loading Libraries
require(dplyr)
require(tidyverse)
require(ggplot2)
require(psych)
require(knitr)
require(olsrr)
require(leaps)
require(kableExtra)
require(data.table)
require(quantmod)
require(forecast)
require(tseries)


#Obtaining the data
getSymbols("aapl")
getSymbols("hon")
Ticker_symbls <- c("AAPL","HON")  ##Ticker symbols of publically listed companies
getSymbols(Ticker_symbls, from = Sys.Date()-365*5 )

#Checking the structure
str(AAPL)
str(HON)

### AR 1 Model
appl_ar_1 <- arima(AAPL$AAPL.Close, order = c(1,0,0))
hnwll_ar_1<- arima(HON$HON.Close, order = c(1,0,0))

## Forecasting
aapl_frcstng <- forecast(appl_ar_1, h = 10)
hnwll_frcstng<- forecast(hnwll_ar_1, h = 10)

## Plotting 
plot(aapl_frcstng, main = "AR(1) Model For Apple")
plot(hnwll_frcstng, main = "AR(1) Model For Honeywell")


## Checking for Stationarity
appl_test <- adf.test(AAPL$AAPL.Close)
print(appl_test)

hon_test <- adf.test(HON$HON.Close)
print(hon_test)

## Auto-Arima fitting for Apple
appl_arima_fit<- auto.arima(AAPL$AAPL.Close)
appl_arima_fit
appl_arima_frcsting <- forecast(appl_arima_fit, h = 8)
plot(appl_arima_frcsting, main="ARIMA Model for Apple")

## Auto-Arima fillting for Honeywell
hnwll_arima_fit<- auto.arima(HON$HON.Close)
hnwll_arima_fit
hnwll_arima_frcsting <- forecast(hnwll_arima_fit, h = 8)
plot(hnwll_arima_frcsting, main = "ARIMA Model for Honeywell")
