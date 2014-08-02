library(XML)
stock="AAPL"
finviz.url <- paste("http://finviz.com/quote.ashx?t=", stock, sep="")
tables <- readHTMLTable(finviz.url)


data=as.data.frame(tables[6])
data=data[13:24,]

final.data=as.data.frame(unlist(cbind(data[,c(2,4,6,8,10,12)])))
final.data=t(final.data)
dim(final.data)

colnames(final.data)=unlist(cbind(data[,c(1,3,5,7,9,11)]))


# set this at the end for all stocks selected
rownames(final.data)
