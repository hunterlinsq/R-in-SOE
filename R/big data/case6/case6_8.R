# State Space Time Series Analysis using dlm package
# Time varying parameters model (TVP)
# China Stock Market Index (Shanghai Common Stock) and
# Renminbi Exchange Rate (CNY/USD)
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
CNY<-Quandl(code="BUNDESBANK/BBEX3_D_CNY_USD_CA_AC_000",type="xts") 
# daily price index: irregularly-spaced time series
# combine data with common rows
data<-merge(SSEC[,"Adjusted Close"],CNY,all=F)
colnames(data)<-c("SSEC","CNY")
# data<-subset(data,time(data)>c("2005-12-31"))
#
Y<-log(data[,"SSEC"])
X1<-data[,"CNY"]
ols1<-lm(Y~X1)
summary(ols1)
#
library(dlm)
# time varying parameters model
tvp1<-function(params,data) {
  dlmModReg(X=data,dV=exp(params[1]),dW=exp(params[2:3]))
}

# ML Estimation 
tvp.fit<-dlmMLE(Y,c(1,1,1),data=X1,build=tvp1)
tvp.fit
tvp.model<-tvp1(tvp.fit$par,X1)
tvp.model
tvp.model$V  # V(ssm1.model)
tvp.model$W  # W(ssm1.model)
tvp.filtered<-dlmFilter(Y,tvp.model)
tvp.smoothed<-dlmSmooth(tvp.filtered)
tvp.resid<-residuals(tvp.filtered)$res
tvp.sm<-dropFirst(tvp.smoothed$s)
ts.plot(tvp.sm)
ts.plot(tvp.sm[,2]) # TVP parameter of X1

acf(tvp.resid)
pacf(tvp.resid)

