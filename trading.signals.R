
library("quantmod")
library("PerformanceAnalytics")
library("TTR")

setwd("D:/stock algorithms")

# References
# http://stackoverflow.com/questions/16964341/r-backtesting-a-trading-strategy-beginners-to-quantmod-and-r
# http://www.quantmod.com/
# DOW Jones ticker list : http://finance.yahoo.com/q/cp?s=%5EDJI+Components

# Load a file of stocks and buy/sell orders with timestamp

ptm <- proc.time() #time the process


process.stock<-function(stock){
  data<-getSymbols(stock,src="yahoo", auto.assign=F) # from yahoo finance 
  adj.close<-data[,6]
  volume<-data[,5]
  
  open<-data[,1]
  high<-data[,2]
  low<-data[,3]
  close<-data[,4]
  
  cat("Data from: ",  
      as.character(time(last(adj.close))), " | ",
      as.character(time(first(adj.close))), "\n")
  
  # Moving Averages
  EMA.10=EMA(adj.close, n = 10)
  EMA.30=EMA(adj.close, n = 30)
  SMA.10=SMA(adj.close, n = 10)
  SMA.30=SMA(adj.close, n = 30)
  
  VWAP.10=VWAP(adj.close, volume, n = 10)
  
  # Volatility
  close.vol=volatility(data, n = 10, calc = "close", N = 260)
  gk.vol   =volatility(data, n = 10, calc = "garman.klass", N = 260)
  rs.vol   =volatility(data, n = 10, calc = "rogers.satchell", N = 260)
  
  
  plot(tail(gk.vol,100))
  
  # Bollinger Bands
  BB.20=BBands(data, n = 20, maType, sd = 2, ...)
 
  
  # combine all the indicators
  output.table<-cbind(adj.close, volume, EMA.10, EMA.30, SMA.10, SMA.30, VWAP.10)
  
    # Define strategy
  output.table$position <- ifelse(adj.close>SMA.30 , 1 , 0)
  
  colnames(output.table)=c("adj.close", "volume", 
                           "EMA.10", "EMA.30", 
                           "SMA.10", "SMA.30",
                           "VWAP", "position")

  myReturn <- lag(output.table$position) * dailyReturn(output.table$adj.close)
  
  charts.PerformanceSummary(cbind(dailyReturn(output.table$adj.close),myReturn))
  
  # Index on this year and sum the returns to get the difference
  
  # Run portfolio Analytics on this to get drawdowns, etc.....
  
  cat(stock, ": finished processing")
  return(output.table)
  
  
}

AAPL<-process.stock("AAPL")

# shorten time frame


table.Stats(test[,1:6])
managers[ trailing36.rows, Rf.col, drop=FALSE])



# optimize allocation

# portfolio TTR


