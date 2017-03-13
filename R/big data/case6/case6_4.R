# China Stock Market Index: Shanghai Common Stock
# Time Series Exponential Smoothing using forecast package
#
# Using Quandl API to search and retrieve data series
# install.packages("Quandl")
library(Quandl)
# Quandl.auth("4UqGASL7CigqHzpfdnNw")
# SP500<-Quandl(code="YAHOO/INDEX_GSPC",type="xts") 
# xts or zoo type should be used for daily high frequency data
# frequency conversion for xts type: apply.xxx
# xxx=yearly, quartertly, monthly, weekly
SSEC<-Quandl(code="YAHOO/INDEX_SSEC",type="xts") 
# daily price index: irregularly-spaced time series
data<-SSEC[,"Adjusted Close"]
summary(data)
plot(data)

# monthly average: regularly-space time series
mdata<-ts(apply.monthly(data,mean),start=c(1997,7),frequency=12)
plot(mdata)

# daily returns in percent
rd<-na.omit(100*diff(log(data)))
rm<-ts(apply.monthly(rd,sum),start=c(1997,7),frequency=12)
plot(rm,main="Monthly Log Returns",ylab="Percent")

# rv<-ts(apply.monthly(rd^2,sum),start=c(1997,7),frequency=12)

# select variable, setup train and test data 
Y<-rm
Y.train<-window(Y,end=c(2014,12))
Y.test<-window(Y,start=c(2015,1))

# ETS Analysis and Forecasts
library(forecast)
# model=(level, trend, seasonal)
# ets1<-ets(Y.train,model="ZZZ")  # automatic model
ets1<-ets(Y.train,model="AAA")
ets1
plot(ets1)

# h-step ahead forecasts
for1<-forecast(ets1,h=30)
for1
plot(for1)
lines(Y.test)
accuracy(for1,Y.test)

# time series cross validation
# 1-step ahead forecasts without re-estimation
ets1a<-ets(Y,model=ets1)  # fit entire data with estimated model
for1a<-window(fitted(ets1a),start=c(2015,1)) # extract the forecasts
for1a
lines(for1a,col="red")
accuracy(for1a,Y.test)

# a function to compute forecast error statistics
predict.error<-function(x,p) {
  data<-ts.intersect(x,p)
  x<-data[,"x"]
  p<-data[,"p"]
  e<-x-p                           # prediction error
  mx<-mean(x)
  mp<-mean(p)
  sx<-sqrt(mean((x-mx)^2))
  sp<-sqrt(mean((p-mp)^2))
  r<-mean((x-mx)*(p-mp))/(sx*sp)
  # r<-cor(x,p)
  MSE<-round(mean(e^2),4)                   # mean squared error
  # results list
  r2<-round(r^2,4)
  ME<-round(mean(e),4)                      # mean error
  MAE<-round(mean(abs(e)),4)                # mean absolute error
  MAPE<-round(100*mean(abs(e/x)),4)         # mean absolute error
  RMSE<-round(sqrt(MSE),4)                  # root mean squared error
  RMSPE<-round(100*sqrt(mean((e/x)^2)),4)   # root mean squared error
  # MSE Decomposition in percent
  Um<-round(100*((mx-mp)^2)/MSE,4)
  Us<-round(100*((sx-sp)^2)/MSE,4)
  Uc<-round(100*(2*(1-r)*sp*sx)/MSE,4)
  Ur<-round(100*((sp-r*sx)^2)/MSE,4)
  Ud<-round(100*((1-r^2)*sx^2)/MSE,4)
  results<-c(ME,MAE,MAPE,MSE,RMSE,RMSPE,Um,Us,Uc,Ur,Ud,r2)
  names(results)<-c("ME","MAE","MAPE","MSE","RMSE","RMSPE","Um%","Us%","Uc%","Ur%","Ud%","r2")
  return(results)
}
# prediction error statistics
predict.error(Y.test,for1$mean)
predict.error(Y.test,for1a)
