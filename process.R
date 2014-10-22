
cat("----------------------------\n")
cat("Processing statistics \n")

# t(table.CalendarReturns(close.daily[,1])
# chart.Boxplot()

# http://stackoverflow.com/questions/13522528/calculate-returns-of-xts-object-with-multiple-columns
daily.returns<-ROC(close.daily)

# Pull returns for last year to calculate stats on against SP500
past.year<-paste(seq(Sys.Date(), length = 2, by = "-1 year")[2], "/", sep="")
daily.return<-daily.returns[past.year]

num.tickers<-ncol(daily.returns)

CAPM.table<-table.CAPM(Ra=daily.returns[,1:(num.tickers-1)], 
                       Rb=daily.returns[,num.tickers],
                       Rf=0)

colnames(CAPM.table)<-gsub(".Adjusted", "", colnames(daily.returns))[1:(num.tickers-1)]


Risk.table<-table.DownsideRisk(daily.returns[,1:(num.tickers-1)],Rf=0)

colnames(Risk.table)<-gsub(".Adjusted", "", colnames(Risk.table))


cat("Risk and CAPM table generated \n")

Risk.table
CAPM.table

# http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR/code3.html
# calculate returns YTD, 6 months, 3 months, 3,2,1 days
# calculate moving averages, normalized vol

cat("Risk and CAPM table generated \n")
cat("----------------------------- \n")


cat("Scraping FINVIZ \n")

process.fin.data<-function(stock){
  
  finviz.url <- paste("http://finviz.com/quote.ashx?t=", stock, sep="")
  tables <- readHTMLTable(finviz.url)
  data=as.data.frame(tables[6])
  data=data[13:24,]
  
  final.data=as.data.frame(unlist(cbind(data[,c(2,4,6,8,10,12)])))
  final.data=t(final.data)
  
  # Check to make sure download is in right format 
  if(ncol(final.data)==72){
    cat(stock, ": processed correctly \n")
  } else (cat(stock, ": !error investigate \n"))
  
  # Format
  colnames(final.data)=unlist(cbind(data[,c(1,3,5,7,9,11)]))
  
  return(final.data)
}


# PASS A VECTOR OF STOCKS
output<-data.frame()
for (i in 1:nrow(portfolio)){
  ticker=portfolio[i,1]
  output<-rbind(output,process.fin.data(ticker) )
  
}


rownames(output)<-portfolio[,1]

head(t(output))


