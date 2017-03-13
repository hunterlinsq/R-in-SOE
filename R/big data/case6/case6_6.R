# Exchange Rate CNY/USD
# State Space Time Series Models Using DLM package
# Using Quandl API to search and retrieve data series
# install.packages("Quandl")
library(Quandl)
# Quandl.auth("4UqGASL7CigqHzpfdnNw")
# SP500<-Quandl(code="YAHOO/INDEX_GSPC",type="xts") 
# xts or zoo type should be used for daily high frequency data
# frequency conversion for xts type: apply.xxx
# xxx=yearly, quartertly, monthly, weekly
CNY<-Quandl(code="BUNDESBANK/BBEX3_D_CNY_USD_CA_AC_000",type="xts") 
# daily price index: irregularly-spaced time series
#CNY<-subset(CNY,time(CNY)>c("2005-12-31"))
summary(CNY)
plot(CNY)

# select variable for analysis
Y<-CNY
Y.train<-window(Y,end=as.Date("2014-12-31"))
Y.test<-window(Y,start=as.Date("2015-01-01"))

library(dlm)
# local level model
dlm1<-function(params) {
  dlmModPoly(order=1,dV=exp(params[1]),dW=exp(params[2]))
}
# constant level: dW=0 restricted
dlm1<-function(params) {
  dlmModPoly(order=1,dV=exp(params[1]),dW=0)
}

# local trend model
dlm2<-function(params) {
  dlmModPoly(order=2,dV=exp(params[1]),dW=exp(params[2:3]),m0=c(level0,slope0))
}
# local trend with constant level: dW[2]=0 restricted
dlm2<-function(params) {
  dlmModPoly(order=2,dV=exp(params[1]),dW=c(0,0),m0=c(level0,slope0))
}

# local trend ARMA model
dlm3<-function(params) {
  dlmModPoly(order=2,dV=exp(params[1]),dW=exp(params[2:3]),m0=c(level0,slope0)) +  
    dlmModARMA(ar=params[5],ma=params[6],sigma2=exp(params[4]))
}

# ML Estimation 
dlm.fit<-dlmMLE(Y,c(1,1),dlm1)
#dlm.fit<-dlmMLE(Y,c(1,1,1),dlm2)
#dlm.fit<-dlmMLE(Y,c(1,1,1,1,0,0),dlm3)
dlm.fit
dlm.model<-dlm1(dlm.fit$par)
#dlm.model<-dlm2(dlm.fit$par)
#dlm.model<-dlm3(dlm.fit$par)
dlm.model
dlm.model$V  # V(dlm1.model)
dlm.model$W  # W(dlm1.model)
dlm.filtered<-dlmFilter(Y,dlm.model)
dlm.smoothed<-dlmSmooth(dlm.filtered)
dlm.resid<-residuals(dlm.filtered)$res
dlm.fm<-dropFirst(dlm.filtered$m)  # filtered mu
dlm.sm<-dropFirst(dlm.smoothed$s)  # smoothed mu
ts.plot(dlm.sm)
ts.plot(dlm.sm[,1])
ts.plot(dlm.sm[,2])
