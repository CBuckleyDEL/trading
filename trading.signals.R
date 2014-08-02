library("quantmod")
library("PerformanceAnalytics")
library("TTR")
library("gridExtra")

# References
# http://stackoverflow.com/questions/16964341/r-backtesting-a-trading-strategy-beginners-to-quantmod-and-r
# http://www.quantmod.com/
# DOW Jones ticker list : http://finance.yahoo.com/q/cp?s=%5EDJI+Components

# Load a file of stocks and buy/sell orders with timestamp

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
  EMA.50=EMA(adj.close, n = 50)
  EMA.100=EMA(adj.close, n = 100)
  
  SMA.10=SMA(adj.close, n = 10)
  SMA.30=SMA(adj.close, n = 30)
  
  VWAP.10=VWAP(adj.close, volume, n = 10)
  
  # Volatility
  close.vol=volatility(data, n = 10, calc = "close", N = 260)
  gk.vol   =volatility(data, n = 10, calc = "garman.klass", N = 260)
  rs.vol   =volatility(data, n = 10, calc = "rogers.satchell", N = 260)
  
  
  # Bollinger Bands
  BB.20=BBands(data[,2:4], n = 20, sd = 2)
  
  
  # combine all the indicators
  output.table<-cbind(adj.close, volume, EMA.10, EMA.30, EMA.50, EMA.100,
                      SMA.10, SMA.30, VWAP.10, BB.20)

  colnames(output.table)=c("adj.close", "volume", 
                           "EMA.10", "EMA.30", "EMA.50", "EMA.100",
                           "SMA.10", "SMA.30",
                           "VWAP","BB.20.down", "BB.20.mavg", "BB.20.up", "BB.20.pctB")

  # Define strategy
  output.table$EMA.10.position <- ifelse(adj.close>output.table$EMA.10 , 1 , 0)
  output.table$EMA.30.position <- ifelse(adj.close>output.table$EMA.30 , 1 , 0)
  output.table$EMA.50.position <- ifelse(adj.close>output.table$EMA.50 , 1 , 0)
  output.table$EMA.100.position <- ifelse(adj.close>output.table$EMA.100 , 1 , 0)
  
  output.table$SMA.10.position <- ifelse(adj.close>output.table$SMA.10 , 1 , 0)
  output.table$SMA.30.position <- ifelse(adj.close>output.table$SMA.30 , 1 , 0)
  
  output.table$BB.20.position <- ifelse(adj.close<output.table$BB.20.down , 1 , 0) # less than BB.20, buy
  
  
  # calculate returns for each strategy
  # NEED to turn this into a function to reduce code
  EMA.10.return <- lag(output.table$EMA.10.position) * dailyReturn(output.table$adj.close)
  EMA.30.return <- lag(output.table$EMA.30.position) * dailyReturn(output.table$adj.close)
  EMA.50.return <- lag(output.table$EMA.50.position) * dailyReturn(output.table$adj.close)
  EMA.100.return<-lag(output.table$EMA.100.position) * dailyReturn(output.table$adj.close)
  
  SMA.10.return <- lag(output.table$SMA.10.position) * dailyReturn(output.table$adj.close)
  SMA.30.return <- lag(output.table$SMA.30.position) * dailyReturn(output.table$adj.close)

  BB.20.return <- lag(output.table$BB.20.position) * dailyReturn(output.table$adj.close)
  
  # Remove NA's from the xts signals
  EMA.10.return=EMA.10.return[!is.na(EMA.10.return)] 
  EMA.30.return=EMA.30.return[!is.na(EMA.30.return)] 
  EMA.50.return=EMA.50.return[!is.na(EMA.50.return)] 
  EMA.100.return=EMA.100.return[!is.na(EMA.100.return)] 
  
  SMA.10.return=SMA.10.return[!is.na(SMA.10.return)] 
  SMA.30.return=SMA.30.return[!is.na(SMA.30.return)] 
  BB.20.return=BB.20.return[!is.na(BB.20.return)] 
  
  #------------------------------------
  # Calculate Returns
  # one month return
  month.EMA.10<-sum(tail(EMA.10.return,20))
  month.EMA.30<-sum(tail(EMA.30.return,20))
  month.EMA.50<-sum(tail(EMA.50.return,20))
  month.EMA.100<-sum(tail(EMA.100.return,20))
  
  month.SMA.10<-sum(tail(SMA.10.return,20))
  month.SMA.30<-sum(tail(SMA.30.return,20))
  
  month.BB.20<-sum(tail(BB.20.return,20))
  

  #------------------------------------
  # PLOTS
  # produce a ggplot barplot for each signals last month returns
  monthly.returns<-c(month.EMA.10, month.EMA.30, month.EMA.50, month.EMA.100, 
                     month.SMA.10, month.SMA.30,
                     month.BB.20)
  
  barplot(monthly.returns, main="Monthly Returns", xlab="Strategies",
          names.arg=c("EMA.10", "EMA.30", "EMA.50", "EMA.100", "SMA.10", "SMA.30", "BB.20"))
  
  # produce a .pdf report with gridExtra for each stock that is analyzed
  
  # Stock Price over last year
  # p1<-plot(tail(output.table$adj.close, 250),major.ticks='months',minor.ticks=FALSE,main=NULL,col=3)
  # p2<-charts.PerformanceSummary(cbind(dailyReturn(output.table$adj.close),EMA.10.return))
  # p3<-plot(cbind(dailyReturn(output.table$adj.close), cumsum(EMA.100.return)))
  # p4<-charts.PerformanceSummary(cbind(dailyReturn(output.table$adj.close),SMA.30.return))
  
  # grid.arrange(p1, arrangeGrob( p3, ncol=1), ncol=2, widths=c(1,1.2))
  
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
