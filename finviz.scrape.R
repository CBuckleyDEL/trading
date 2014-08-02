library(XML)

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
output<-rbind(process.fin.data("AAPL"),
              process.fin.data("BAX"),
              process.fin.data("DE"),
              process.fin.data("JNK"),
              process.fin.data("IP"),
              process.fin.data("PG"),
              process.fin.data("AMZN"),
              process.fin.data("MCD"),
              process.fin.data("F"),
              process.fin.data("JNJ"),
              process.fin.data("BMY"),
              process.fin.data("SIRI"),
              process.fin.data("KO"))

rownames(output)<-c("AAPL", "BAX", "DE", "JNK", "IP", "PG", 
                    "AMZN", "MCD", "F", "JNJ", "BMY","SIRI", "KO")
