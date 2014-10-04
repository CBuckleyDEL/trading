# Define the portfolio Holding
# need to fully develop this more

# need to add sector analysis

# sector ETF's
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
tickers.desc = spl('ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities')



portfolio <- data.frame(ticker = character(0), shares = numeric(0), stringsAsFactors=F)

portfolio[1,]=c("AAPL", 191)
portfolio[2,]=c("BAX", 22)
portfolio[3,]=c("AMZN", 4)
portfolio[4,]=c("SIRI", 2572)
portfolio[5,]=c("JNK", 86)
portfolio[6,]=c("BMY", 125)
portfolio[7,]=c("DE", 19)
portfolio[8,]=c("KO", 66)
portfolio[9,]=c("F", 400)
portfolio[10,]=c("MCD", 25)
portfolio[11,]=c("PG", 20)
portfolio[12,]=c("JNJ", 58)
portfolio[13,]=c("T", 55)
portfolio[14,]=c("IP", 40)
