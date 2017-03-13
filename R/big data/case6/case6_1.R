# China Stock Market Index: Shanghai Common Stock
# Time Series Data Visualization
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
# the following is for saving to csv and retriving from it
# SSEC<-Quandl(code="YAHOO/INDEX_SSEC",type="raw",order="asc") 
# write.csv(SSEC,file="C:/Course16/WISE2016/data/SSEC.csv",row.names=F)
# SSEC1<-read.csv(file="C:/Course16/WISE2016/data/SSEC.csv",stringsAsFactors=F)
# SSEC1<-xts(SSEC1,order.by=as.Date(SSEC1$Date))
# data<-SSEC1[,"Adjusted.Close"]

data<-SSEC[,"Adjusted Close"]
summary(data)
plot(data)
# monthly average: regularly-space time series
mdata<-ts(apply.monthly(data,mean),start=c(1997,7),frequency=12)
plot(mdata)

library(forecast)
monthplot(mdata)
seasonplot(mdata,s=12,year.labels=T,col=rainbow(12))
lag.plot(mdata,12)

# daily return as difference of log price index
rd<-na.omit(100*diff(log(data)))
summary(rd)
plot(rd)
# monthly return is the sum of daily returns in a month
rm<-ts(apply.monthly(rd,sum),start=c(1997,7),frequency=12)
plot(rm)
monthplot(rm)
seasonplot(rm,s=12,year.labels=T,col=rainbow(12))
lag.plot(rm,12)
Acf(rm,60,type="correlation")
Acf(rm,60,type="partial")

# monthly volatility is the sum of daily squared returns in a month
rv<-ts(apply.monthly(rd^2,sum),start=c(1997,7),frequency=12)
plot(rv)
monthplot(rv)
seasonplot(rv,s=12,year.labels=T,col=rainbow(12))
lag.plot(rv,12)
Acf(rv,60,type="correlation")
Acf(rv,60,type="partial")

