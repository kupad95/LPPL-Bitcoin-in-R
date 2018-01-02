#Delete everything until now
rm(list=ls())

setwd("C:/Users/K Upadhyay/Dropbox/Bitcoin/lppl_bitcoin-master/lppl_bitcoin-master")
fileName <- './data/coindesk.csv'
ticker <- read.csv(fileName, header=TRUE, sep=",")
ticker$Date <- as.Date(ticker$Date, format = "%m/%d/%Y")

library('stats')
summary(ticker$Close)
qqnorm(ticker$Close)
qqline(ticker$Close)
acf(ticker$Close)

returns <- diff(ticker$Close)/ticker$Close[-length(ticker$Close)]
returnticker <- data.frame(ticker$Date[-length(ticker$Date)],returns)
plot(returnticker$ticker.Date..length.ticker.Date.., returnticker$returns, xlab="Date", ylab="Returns", typ='l')

summary(returnticker$returns)
qqnorm(returnticker$returns)
qqline(returnticker$returns)
acf(returnticker$returns)
sd(returnticker$returns)

hist(returnticker$returns)

#Compare to GOLD = GLD
library('PerformanceAnalytics')
library('zoo')
library('tseries')

GOLD.prices = get.hist.quote(instrument="GLD", start="2010-07-18",
                             end="2017-01-12", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="d", retclass="ts")
goldticker <- data.frame(GOLD.prices)

GOLDreturns <- diff(GOLD.prices)/GOLD.prices[-length(GOLD.prices)]
GOLDreturnticker <- data.frame(GOLDreturns)

#Compare to Oil
library(Quandl)
oil = Quandl("OPEC/ORB", order="asc", start_date="2010-07-18", end_date="2017-01-12")

alldates <- data.table(date=seq.Date(min(oil$Date), max(oil$Date), by="day"))
dt2 <- merge(oil, alldates, by.x='Date', by.y='date', all=TRUE)

OILreturns <- diff(dt2$Value)/dt2$Value[-length(dt2$Value)]
OILreturnticker <- data.frame(OILreturns)

#Compare to US Real Estate Index
RealEstate = get.hist.quote(instrument="IYR", start="2010-07-18",
                       end="2017-01-12", quote="AdjClose",
                       provider="yahoo", origin="1970-01-01",
                       compression="d", retclass="ts")
RealEstateticker <- data.frame(RealEstate)
RealEstatereturns <- diff(RealEstate)/RealEstate[-length(RealEstate)]
RealEstatereturnticker <- data.frame(RealEstatereturns)


#Compare to S&P500
SP500 = get.hist.quote(instrument="^GSPC", start="2010-07-18",
                             end="2017-01-12", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="d", retclass="ts")
SP500ticker <- data.frame(SP500)
SP500returns <- diff(SP500)/SP500[-length(SP500)]
SP500returnticker <- data.frame(SP500returns)

#Compare to 10 yr US Treasuries
library(readr)
bondetf <- read_csv("C:/Users/K Upadhyay/Downloads/bondetf.csv", 
                    +     col_types = cols(Date = col_date(format = "%d/%m/%Y")))
alldates <- data.table(date=seq.Date(min(tenyr$Date), max(tenyr$Date), by="day"))
dt3 <- merge(bondetf, alldates, by.x='Date', by.y='date', all=TRUE)

bondreturns <- diff(dt3$Value)/dt3$Value[-length(dt3$Value)]
bondreturnticker <- data.frame(bondreturns)

#Set Up Plot 1 - BTC vs Oil, Gold and Real Estate
plot(returnticker$returns, xlab="Date", ylab="BTCUSD", typ='l', col="red")
lines(GOLDreturnticker$AdjClose, col="blue")
lines(OILreturnticker$AdjClose, col="yellow")
lines(RealEstatereturnticker$AdjClose, col="green")

#Set Up Plot 2 - BTC vs S&P500, US Bonds
plot(returnticker$returns, xlab="Date", ylab="BTCUSD", typ='l', col="red")
lines(SP500returnticker$AdjClose, col="blue")
lines(bondreturnticker$bondreturns, col="green")

#Fix dataframes for corr analysis

oilpricefixed <- na.locf(dt2)
goldpricefixed <- na.locf(goldticker)
realestatepricefixed <- na.locf(RealEstateticker)
SP500pricefixed <- na.locf(SP500ticker)
bondpricefixed <- na.locf(dt3)

write.csv(bondpricefixed, file = "C:/Users/K Upadhyay/Dropbox/Bitcoin/R Code/blanksheet.csv")

#IF NEEDED - GBPUSD
library('padr')
library('dplyr')
library('data.table')
library(readr)
gbpusd <- read_csv("C:/Users/K Upadhyay/Dropbox/Bitcoin/Data/gbpusd.csv", 
                   +     col_types = cols(DATE = col_date(format = "%d/%m/%Y")))
#Correct GBPUSD for missing dates
alldates <- data.table(date=seq.Date(min(gbpusd$DATE), max(gbpusd$DATE), by="day"))
dt <- merge(gbpusd, alldates, by.x='DATE', by.y='date', all=TRUE)
GBPUSDreturns <- diff(dt$gbpusd)/dt$gbpusd[-length(dt$gbpusd)]
GBPUSDreturnsticker <- data.frame(GBPUSDreturns)
