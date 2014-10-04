# see README file for more information
# date  : 9-27-2014


# Set home directory
setwd("/Users/shawnpope/Desktop/stocks")

# Scripts
source("load.packages.R")
source("portfolio.holdings.R") # defines the table with holdings and position sizes

source("finviz.scrape.R") # scrapes finviz.com for stock/etf info
# returns a df called output - use this to get financial information - not a price source

# NEED to create a script which determines the portfolio beta and div yield
# also emails it out daily

Portfolio.value=sum(as.numeric(portfolio$shares)*as.numeric(as.character(output$Price)))
Portfolio.beta=sum(as.numeric(as.character(output$Beta))*(as.numeric(portfolio$shares)*as.numeric(as.character(output$Price))/Total.portfolio.value), na.rm=TRUE)

cat("Total Portfolio value: ", Portfolio.value, "\n")
cat("Total Portfolio value: ", Portfolio.beta, "\n")


# Options Analysis
source("options.R")
# functions include BS(), implied.vol(), return.option()

opt.example<-return.option("AAPL") # -> returns data$calls, data$puts
output<-process.option(opt.example, 0.03, 30) # first input is how near the strike price, need to add a days expire input

# near the money option data
head(output$calls)
output$puts

ticker="AAPL"
str_split(rownames(head(output$calls))[1], ticker)

strsplit(rownames(head(output$calls))[1], ticker)


# AAPL 150417 C00585000
# TICKER yymmdd
# TO DO: parse the option name using the ticker and stringr package
# this will fix the weekly problem
# http://en.wikipedia.org/wiki/Option_symbol

# need a function to process the returned option data - pass parameters
# about downside protection


