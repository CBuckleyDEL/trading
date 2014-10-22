# Author: Shawn Pope
# Date: 10/18/2014
#
# Purpose: process data from portfolio table
# run through Performance Analytics to capture beta to SP500

tickers<-portfolio[,1]
close.daily<-data.frame()

for (i in 1:length(tickers)){
  output<-getSymbols(tickers[i],src="yahoo", auto.assign=F)[,6]
  close.daily<-cbind(close.daily, output)
}

# roll back xts subseting to beginning of clean data


cat("Tickers with NA's: \n")
incomplete.col=which(sapply(close.daily, function(x) sum(is.na(x)))>0)


cat("Truncating data to 2012 \n")
close.daily<-close.daily["2012/"]


incomplete.col=which(sapply(close.daily, function(x) sum(is.na(x)))>0)
 if (length(incomplete.col)==0){
   cat("No NA's present in the data \n")
 } else {
   cat("Still tickers with missing data. Check close.daily \n")
   colnames(close.daily)[incomplete.col]
 }

