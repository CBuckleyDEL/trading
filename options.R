# 8/27/2013
# author: Shawn Pope
# email: shawnpope@gmail.com

library(RCurl)
library(XML)
library(tis)
library(quantmod)
library(RQuantLib)
library(zoo)
library(timeDate)

setwd("D:/stock algorithms")
source("stock.functions.R")
# greeks(488.59,490,0.04,0.025,23,.3)

# greeks<-function(S,X,b,r,Time,v)
# implied.vol <- function(S, K, T, r, market, type)
# BS <- function(S, K, T, r, sig, type="C")

# Returning Option Expiration Dates
# adjust for holiday, April 18/19
# http://www.marketwatch.com/optionscenter/calendar/2014

rm(list = ls())

return.option<-function(ticker){
  
  # Calculate the dates of the 3rd Friday of the month
  tS<-timeSequence(from = "2014-01-01", to = "2016-01-01", by = "month")
  Friday.date<-strftime(timeNthNdayInMonth(tS, nth= 3, nday = 5))
  Friday.month<-strftime(strptime(Friday.date, format="%Y-%m-%d"), format="%b %Y")
  Friday.date<-cbind(Friday.month, Friday.date)
  
  # Download Option Data
  option.data<-getOptionChain(ticker, NULL)
  num.months<-length(attributes(option.data)$names)
  
  price.last=getQuote(ticker, src="yahoo")$Last
  
  calls<-data.frame()
  puts<-data.frame()
  
  for (x in 1:num.months){
    
    calls.frame<-as.data.frame(option.data[[x]][1])
    puts.frame<-as.data.frame(option.data[[x]][2])
    
    calls.frame$date=attributes(option.data)$names[x]
    calls.frame$Last=price.last
    
    puts.frame$date=attributes(option.data)$names[x]
    puts.frame$Last=price.last
    
    calls<-rbind(calls, calls.frame)
    puts<-rbind(puts, puts.frame)  
  } 
  
  call.data.index=row.names(calls)
  put.data.index=row.names(puts)
  
  # Merge the 3rd Friday dates
  calls<-merge(calls, Friday.date, by.x = "date", by.y = "Friday.month", all.x=T)
  puts<-merge(puts, Friday.date, by.x = "date", by.y = "Friday.month", all.x=T)
  
  rownames(calls)=call.data.index
  rownames(puts)=put.data.index
  
  
  # Subset options based on VOLUME to clean up table
  #Option Price: average of bid/ask
  calls$Option.Price.Mid=(calls$calls.Bid+calls$calls.Ask)/2
  puts$Option.Price.Mid=(puts$puts.Bid+puts$puts.Ask)/2

  
  calls$Moneyness=apply(cbind(calls$Last-calls$calls.Strike, 0), 1, max) 
  puts$Moneyness=apply(cbind(puts$Last-puts$puts.Strike, 0), 1, max) 
  
  # merge the calls and puts together in 1 dataframe
  return(list(calls=calls, puts=puts))
}


data<-return.option("AAPL")
calls<-data$calls[data$calls$calls.OI>10,]
puts<-data$puts[data$puts$puts.OI>10,]

# subset the calls and puts
# select upcoming options and ones near the money


puts<-data$puts



# Calculate Implied Volatility - need to compare against actual vol (30days prices)
T=as.numeric(as.Date(calls$Friday.date[100], format="%Y-%m-%d")-Sys.Date())
S=calls$Stock.Price[100]
K=calls$calls.Strike[100]
r=0.0025
market=calls$calls.Last[100]
type="C"

implied.vol(S, K, T, r, market, type)

implied.vol(S=535.96, K=550, T=65, r=0.25, market=25.47, type="C")

data<-return.option("AA")
calls<-data$calls
puts<-data$puts



#--------------------------------------------------------



plot(options$calls$Strike, options$calls$Last, type="l", col=2)
points(next.options$calls$Strike, next.options$calls$Last, type="l", col=3)

total<-do.call(rbind, lapply(aapl_total, function(x) do.call(rbind, x)))

#strsplit(rownames(total)[1], ".")[[1]]
write.table(total, 'data_total.txt', sep=" ")


# Return the At the Money Call
for (x in 1:length(stock.list)) {
  
  price.last<-getQuote(stock.list[x], src="yahoo")$Last
  calls<-getOptionChain(stock.list[x])$calls
  atm.call<-calls[which.min(abs(price.last-calls$Strike)),]$Last
  strike.price<-calls[which.min(abs(price.last-calls$Strike)),]$Strike
  days.expire<-as.integer(option.expire-Sys.Date())
  
  #calculate intrinsic value
  if(strike.price>price.last){
    intrinsic.val<-atm.call
  } else {intrinsic.val<-atm.call + strike.price - price.last}
  
  cat("--------------------\n")
  cat(stock.list[x],"\n")
  cat("Last Price: ", price.last, "\n")
  cat("Strike Price: ", strike.price, "\n")
  cat(stock.list[x], "ATM Call: ", atm.call, "\n")
  cat("Expire(days): ", days.expire , "\n")
  cat("Intrinsic Value: ", intrinsic.val, "\n")
  cat("Extrinsic Value: ", atm.call-intrinsic.val, "\n")
  
  # fixed the percentage based on intrinsic value
  cat("% called     : ", (1+intrinsic.val/strike.price)^(365/days.expire) - 1 , "\n")
  cat("% not called : ", (1+intrinsic.val/price.last)^(365/days.expire) - 1 , "\n")
  
  cat("--------------------\n")
  
}
