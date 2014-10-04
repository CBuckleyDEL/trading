
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


print(xtable(output), type="html", file="example.html")
