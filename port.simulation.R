# Simulate the portfolio of stocks to determine portfolio VaR limits

# http://www.rfortraders.com/lecture-6-stochastic-processes-and-monte-carlo/

# TO DO!
# load the portfolio that is created earlier
# create the covariance matrix using exponential weighted returns

# Calculate the total return of the portfolio over the time period
# store return and resample portfolio
# aggregate/plot/and determine the 5% time period VaR


require(MASS)
require(quantmod)
 
 #load a few symbols into memory
 getSymbols(c("AAPL", "SIRI", "DE", "GOOG", "CVX"))
 
 #compute price matrix
 pM <- cbind(AAPL[,6], SIRI[,6], DE[,6], GOOG[,6], CVX[,6])
 
 #compute returns matrix
 rM <-  apply(pM,2,function(x) diff(log(x)))
 
 #look at pairwise charts
 pairs(coredata(rM))
 
 #compute the covariance matrix
 covR <- cov(rM)
 
 #use this covariance matrix to simulate normal random numbers
 #that share a similar correlation structure with the actual data
 meanV <- apply(rM, 2, mean)
 rV    <- mvrnorm(n = nrow(rM), mu = meanV, Sigma = covR)
 
 #simulate prices based on these correlated random variables
 
 #calculate mean price
 p0 <- apply(pM,2,mean)
 sPL <- list()
 for(i in 1:ncol(rM)){
     sPL[[i]] <-round(p0[i]*exp(cumsum(rV[,i])),2)
 }
 
 #plot simulated prices
 par(mfrow = c(3,2)) 
 plot(sPL[[1]],main="AAPLsim",type="l")
 plot(sPL[[2]], main = "SIRI sim",type = "l")
 plot(sPL[[3]], main = "DE sim", type = "l") 

