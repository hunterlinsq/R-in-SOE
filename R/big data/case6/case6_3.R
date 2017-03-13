# China Stock Market Index: Shanghai Common Stock
# Time Series Exponential Smoothing, using structTS
#
# Using Quandl API to search and retrieve data series
# install.packages("Quandl")
library(Quandl)
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

# using StrucTS
sts1<-StructTS(Y.train,"level")
sts1$coef
plot(sts1$fitted)

sts2<-StructTS(Y.train,"trend")
sts2$coef
plot(sts2$fitted)

sts3<-StructTS(Y.train,"BSM")
sts3$coef
plot(sts3$fitted)

# h-step ahead forecasts
for1<-predict(sts1,n.ahead=30)
for2<-predict(sts2,n.ahead=30)
for3<-predict(sts3,n.ahead=30)

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
predict.error(Y.test,for1$pred)
predict.error(Y.test,for2$pred)
predict.error(Y.test,for3$pred)

plot(Y.test)
lines(for3$pred)
lines(for2$pred,col="green")
lines(for1$pred,col="red")

