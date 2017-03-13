# China Stock Market Index: Shanghai Common Stock
# Structural Time Series (StrucTS function)
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

# daily return as difference of log price index
rd<-na.omit(100*diff(log(data)))
summary(rd)
plot(rd)
# monthly return is the sum of daily returns in a month
rm<-ts(apply.monthly(rd,sum),start=c(1997,7),frequency=12)
# monthly volatility is the sum of daily squared returns in a month
rv<-ts(apply.monthly(rd^2,sum),start=c(1997,7),frequency=12)

Y<-rv
summary(Y)

# Exponential Smoothing: Holt-Winters Filtering
sts1<-decompose(Y)
plot(sts1)
HoltWinters(Y)
HoltWinters(Y,beta=F,gamma=F)

# Exponential Smoothing: Structural TS
# local level model
sts1a<-StructTS(Y,type="level",init=c(1,1))
sts1a
plot(Y)
lines(sts1a$fitted,col="red")
# local linear trend model
sts1b<-StructTS(Y,type="trend",init=c(1,1,1))
sts1b
plot(Y)
lines(sts1b$fitted[,1],col="red")
# best structural model (linear trend and seasonal model)
sts1c<-StructTS(Y,type="BSM")
sts1c
plot(Y)
lines(sts1c$fitted[,1],col="red")

# plot of slope of the trend model
sts1b$fitted
plot(sts1b$fitted[,2])

# plot of slope and seasonal of BSM
sts1c$fitted
plot(sts1c$fitted[,2])
plot(sts1c$fitted[,3])
