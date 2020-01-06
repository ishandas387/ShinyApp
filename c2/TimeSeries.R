library(tseries) 

library(forecast) 

AP= AirPassengers

class(AP) # check the form of class 



View(AP) 

str(AP) # structure of data 



plot(AP)  # time series 

abline(reg=lm(AP~time(AP)), col="blue") # line for the mean of time series 

# make the stationary in mean 



SAP=diff(AP) 

plot(SAP) 

#Stationary in Variance  

LSAP= diff(log(AP)) 

plot(LSAP) 

# autocorrelation (acf) plot  

# partial outocorrelation (pacf) plot 

#AR I MA
#p  d  q
acf(LSAP, lag.max=20) # q=1 for nonstationary ts 

pacf( LSAP, lag.max=20)# p=0 # for nonstationary ts 

## manual modelling using acf and pacf 

#seasonal=list(order=c(0,1,1)) 

manual.fit<-arima(AP, c(0,1,1))  # fitted model 

manual.fit 











manual.fcast <- forecast(manual.fit, h=20) # prediction for 10 step ahead 

plot(manual.fcast) 

# automodelling using auto.arima 

auto.fit<-auto.arima(AP, seasonal=T) 

auto.fit 

auto.fcast <- forecast(auto.fit, h=40) 

plot(auto.fcast) 
