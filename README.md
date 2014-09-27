trading
=======

EOD open source trading algos


Date: 9/27/2014

#
# Purpose:
# The idea is to create a system for portfolio optimization and option pricing analysis
# and to generate reports on the fly, including daily emails with analysis
#
# Things to work on:
# 1) built out Master.R file to control everything
# 2) feed the system from a .csv file that contains all the parameters (etc. stock tickers)
# this should make modifications on the fly very simple
# 3) fix fin.scrape.R - add the ability to pass a list of tickers/etf - ideally through a .csv/text file
# 4) calculate implied vol of option data and compare to realized volatility
# https://www.quantnet.com/threads/realized-volatility-calculation.4914/


# Scripts
load.packages.R - loads all the necessary packages for the financial analysis
fin.scrape.R - scrapes finviz.com for stock/etf info - NEED TO CLEAN THIS UP


# Options Analysis
options.R
# functions include
# BS(), implied.vol()
# return.option() - this a long function which downloads all call and put information available
# returns two df's calls and puts
# Example: data<-return.option("AAPL) -> returns data$calls, data$puts



