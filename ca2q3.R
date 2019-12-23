library(tseries) 
library(forecast)
library(rtsdata)

output<- ds.getSymbol.yahoo('ibm',from='2019-10-01')

class(output) # check the form of class 

View(output) 

str(output) # structure of data 

plot(output)  # time series 

#optimize model
abline(reg=lm(output$IBM.Close~time(output$IBM.Close)), col="blue") # line for the mean of time series 

# make the stationary in mean


manual.fit<-arima(output$IBM.Close, c(2,1,1))  # fitted model 

manual.fit 

manual.fcast <- forecast(manual.fit, h=10) # prediction for 10 step ahead 

plot(manual.fcast) 

