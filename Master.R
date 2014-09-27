# see README file for more information
# date  : 9-27-2014


# Set home directory
setwd("/Users/shawnpope/Desktop/stocks")

# Scripts
source("load.packages.R")
source("fin.scrape.R") # scrapes finviz.com for stock/etf info
# returns a df called output - use this to get financial information - not a price source

# Options Analysis
source("options.R")
# functions include BS(), implied.vol(), return.option()

opt.example<-return.option("AAPL") # -> returns data$calls, data$puts
output<-process.option(data, 0.05)

# near the money option data
output$calls
output$puts

# need a function to process the returned option data - pass parameters
# about downside protection


