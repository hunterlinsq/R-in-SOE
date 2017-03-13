#################################################
#Chapter 12 Financial data analysis using R (2015/06/01) Haifeng Xu
#################################################
#################################################
#1 "Quantmod" package
########################
#1.1 How to get financial data using R

#http://www.quantmod.com

library("quantmod")
#Extract
getSymbols("^GSPC",src="yahoo",from="1994-1-1",to=Sys.Date())
print(head(GSPC));print(tail(GSPC))
print(class(GSPC))
print(is.OHLC(GSPC))
print(is.OHLCV(GSPC))
print(has.OHLC(GSPC,which=FALSE))
print(has.OHLC(GSPC,which=TURE))
has.Vo(GSPC)
Vo(GSPC)
Ad(GSPC)
first(GSPC,5)
last(GSPC,5)
Next(GSPC,1)
#Calculate
Delt(Op(GSPC),type=("arithmetic"))
Delt(Op(GSPC),type=("log"))
Delt(Op(GSPC),Cl(GSPC))
#Translate
first(GSPC,10)
head(to.weekly(GSPC))
to.monthly(GSPC)
#It is possible with one quantmod function to load data from a variety of sources, including...

#Yahoo! Finance (OHLC data)

#Federal Reserve Bank of St. Louis FRED (11,000 economic series)

#Google Finance (OHLC data)

#Oanda, The Currency Site (FX and Metals)www.oanda.com
getSymbols("YHOO",src="google") # from google finance
getSymbols("DEXJPUS",src="FRED") # FX rates from FRED
#Use a function to store the stock code
setSymbolLookup(CJSY=list(name="0001.HK",src="yahoo")
                getSymbols("CJSY",from="1900-1-1",to=Sys.Date()) 
                print(head(CJSY));print(tail(CJSY))
#Download several different stock prices
szSymbols <- c("MSFT","ORCL","GOOG","INTL","AAPL","CSCO","SYMC","TSLA") 
getSymbols(szSymbols,src="yahoo",from="2008-1-1",to=Sys.Date())
#Bond
getSymbols("^TNX",src="yahoo",from="1900-1-1",to=Sys.Date())
print(head(TNX));print(tail(TNX))
#Fund
getSymbols("ACWI",src="yahoo",from="1900-1-1",to=Sys.Date())
print(head(ACWI));print(tail(ACWI))
#Exchange rate
getFX("USD/JPY")
print(head(USDJPY));print(tail(USDJPY))
getSymbols("EUR/USD",src="oanda")
print(head(EURUSD));print(tail(EURUSD))
#Get the stock price
tmp <- getQuote("AAPL");print(tmp);print(class(tmp))
#Get the earnings information of a company
getFinancials("TSLA")
viewFin(TSLA.f)
viewFin(TSLA.f,"CF","A")
#Get the history of stock dividends
getDividends("AAPL")
#Stock split information
getSplits("BIDU")
#Get the financial statement
getFinancials("AAPL")
viewFinancials(AAPL.f)

#1.2 Charting with quantmod
#1.2.1 plot
#suit for one index
getSymbols("^GSPC",src="yahoo",from="2013-1-1",to="2014-1-1")
plot(GSPC)
#1.2.2 A Series of Chart
#chartSeries
chartSeries(GSPC)
chartSeries(GSPC,name="GSPC BARCHART",subset="2013-10-01::2013-10-23",type="bars")
chartSeries(GSPC,name="GSPC LINECHART",subset="2013-10-01::2013-10-23",type="line")
chartSeries(GSPC,name="GSPC LINECHART",subset="2013-10-01::2013-10-23",type="candlesticks")
#rechart
reChart(type="bars",subset="2013-10-05::2013-10-29", show.grid=TRUE)
#barChart
barChart(GSPC,theme="black",subset="first 10 weeks",bar.type="ohlc")
barChart(GSPC,theme="black",subset="first 10 weeks",bar.type="hlc")
#candleChart
candleChart(GSPC,theme="white",subset="2013-10-05::2013-10-30")
candleChart(GSPC,theme="white",subset="2013-10-05::2013-10-30",multi.col=T)
#lineChart
lineChart(GSPC,theme="white",subset="2013-10-05::2013-10-30")
lineChart(GSPC,theme="white",subset="2013-10-05::2013-10-30",line.type="l") lineChart(GSPC,theme="white",subset="2013-10-05::2013-10-30",line.type="p") lineChart(GSPC,theme="white",subset="2013-10-05::2013-10-30",line.type="b")
#Technical index
#ADX: Average Directional Index
chartSeries(GSPC,name="GSPC CANDLECHART",subset="2013-06::2013-10-23",type="candlesticks")
addADX()
#1.2.3 details of chartSeries
windows()
chartSeries(GSPC, name="GSPC", type="candlesticks",
            subset="2012-6/2013-6", TA=NULL, theme=chartTheme("white"))
#1.2.4 change theme parameter
theme.white <- chartTheme("white")
names(theme.white)
theme.white$up.col <- "red"
theme.white$dn.col <- "white"
theme.white$border <- "lightgray"
windows()
chartSeries(GSPC, name="GSPC", type="candlesticks",
            subset="2013-6/", TA=NULL, theme=theme.white)
#1.2.5 choose subset using words
windows()
chartSeries(GSPC, name="GSPC", show.grid = T, type="candlesticks",
            subset="last 3 months", TA="addVo()", theme=theme.white)
#1.2.6 rechart
reChart(theme=chartTheme("black"), subset="last 6 months")
#1.2.7 add several technical indexes
windows()
chartSeries(GSPC, name="GSPC", show.grid = T, type="candlesticks",
            subset="last 2 quarters", TA="addVo();addSMA(20);
            addBBands(20,3)", theme=theme.white)
addMACD()
zoomChart("2013-9")
addCCI(20)
#1.2.8 saving
jpeg("GSPC.jpeg")
chartSeries(GSPC, name="GSPC", show.grid = T, type="candlesticks",
            subset="last 2 quarters", TA="addVo();addSMA(20);
            addBBands(20,3)", theme=theme.white)
dev.off()
#1.3 Example
getSymbols("AAPL",src="yahoo",from="1994-1-1",to=Sys.Date())
AAPL.Cl<-Delt(Cl(get("AAPL")),type=("arithmetic"))
AAPL.Cl[which(abs(AAPL.Cl)>0.02),]
plot(AAPL.Cl)
periodReturn(AAPL,period="daily")
dailyReturn(AAPL)
periodReturn(AAPL,period="weekly")
weeklyReturn(AAPL)
first(allReturns(AAPL),15)

#ASSET RETURNS
library(quantmod)
getSymbols("DEXUSEU",src="FRED") #Obtain exchange rates from FRED
head(DEXUSEU)
tail(DEXUSEU)
USEU.rtn=diff(log(DEXUSEU$DEXUSEU)) # Compute changes
chartSeries(DEXUSEU,theme="white")
chartSeries(USEU.rtn,theme="white")

#VISUALIZATION OF FINANCIAL DATA
library(fBasics)
da=read.table("d-mmm-0111.txt",header=T) # Load data
mmm=da[,2] # Locate 3M simple returns
#1
hist(mmm,nclass=30) # Histogram bin=30
#2
d1=density(mmm)  # Obtain density estimate
range(mmm)  # Range of 3M returns
x=seq(-.1,.1,.001) # Create a sequence of x with increment 0.001.
y1=dnorm(x,mean(mmm),stdev(mmm))
plot(d1$x,d1$y,xlab='rtn',ylab='density',type='l')
lines(x,y1,lty=2)
#3
library(quantmod)
getSymbols("AAPL",from="2011-01-03",to="2011-06-30")
X=AAPL[,1:4] # Locate open, high, low, and close prices 
xx=cbind(as.numeric(X[,1]),as.numeric(X[,2]),as.numeric(X[,3]),as.numeric(X[,4]))
source("ohlc.R")# Compile the R script 
ohlc_plot(xx,xl="days",yl="price",title="Apple Stock")
#4
source("ma.R")  # Compile R script
getSymbols("AAPL",from="2010-01-02",to="2011-12-08")
x1=as.numeric(AAPL$AAPL.Close) # Locate close price ma(x1,21)
#5
da=read.table("m-ibmsp-2611.txt",header=T)
head(da)
ibm=log(da$ibm+1) # Transform to log returns
sp=log(da$sp+1)
tdx=c(1:nrow(da))/12+1926 # Create time index
opar<-par(no.readonly = TRUE)
par(mfcol=c(2,1))
plot(tdx,ibm,xlab='year',ylab='lrtn',type='l')
title(main='(a) IBM returns')
plot(tdx,sp,xlab='year',ylab='lrtn',type='l') # X-axis first.
title(main='(b) SP index')
par(opar)
##################################################################






########################
end
########################








