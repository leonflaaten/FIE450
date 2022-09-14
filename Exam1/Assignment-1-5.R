rm(list=ls())
NHY = read.csv("NHY.OL.csv")

#Loading the data and renaming the columns.
names(NHY) = c("NA","NA2","Date","Long Term Debt","Current Liabilities","Close","Shares Outstanding")

#Cleaning data
NHY = NHY[-1,]
NHY$Date = as.Date(NHY$Date,format="%d/%m/%Y")
NHY = NHY[order(NHY$Date),]

# Strike price is current liabilities plus half long term liabilities.
NHY$StrikePrice = (0.5*as.numeric(NHY$`Long Term Debt`)+as.numeric(NHY$`Current Liabilities`))
#Equity is stock price multiplied with shares outstanding
NHY$Equity = as.numeric(NHY$Close)*as.numeric(NHY$`Shares Outstanding`)

# Dropping all columns except the columns we need.
NHY = NHY[,c("Date","Close","StrikePrice","Equity")]

# Computing the equity return, to be able to compute equity volatility.
NHY$Equity_r = c(NA,diff(log(NHY$Equity)))

# Input variables
rf = 0.01
sigma=sd(NHY$Equity_r,na.rm=TRUE)*sqrt(250)
SO = K = NHY$StrikePrice[1]
n = length(NHY$Date)
T = 1

# To find the asset values we use the inverse of the Black-Scholes formula.
# We calculate the Asset value with respect to the call price.
# We use equity as call value, equity volatility as sigma.
# As a starting value for S0 we use the strike value.
# Strike value as strike price.

Inverse_BS_call = function(C, S0, sigma, K, rf, T) {
  d1 = (log(S0/K) + (rf + sigma^2/2)*T)/sigma/sqrt(T)
  d2 = d1 - sigma*sqrt(T)
  S0 = (C + exp(-rf*T)*K*pnorm(d2))/pnorm(d1)
  return(S0)
}

# We create a column for the Asset values.
NHY$Asset = 0

#Computing the first asset value
NHY$Asset[1] = Inverse_BS_call(NHY$Equity[1], SO, sigma, K, rf, T)

# Using a for-loop to create a time series for the asset values.
for (i in 2:length(NHY$Date)) {
    NHY$Asset[i] = Inverse_BS_call(NHY$Equity[i], NHY$Asset[i-1], sigma, NHY$StrikePrice[i], rf, T)
  }


# since we just set the starting value for assets equal strike price, we remove this value.
# because it is not likely a correct estimate, and will not give us a precise sigma.
NHY = NHY[-1,]
row.names(NHY) = NULL

# Estimated time series of asset values. 
V = NHY$Asset


##########################
# Amount of observations
n = length(NHY$Date)

# Computing "mu" with the first asset value...
# ... and the last asset value to find the mean.
u = (1/n)*(log(NHY$Asset[length(NHY$Date)])-log(NHY$Asset[1]))

# Function to calculate asset volatility,
# We use a for loop to calculate the sum fuction inside the volatility function
Sigma.fun = function(u) {
  vol = 0
  for (i in 1:(n-1)) {
    sum = (log(NHY$Asset[i+1]/NHY$Asset[i])-u)^2
    vol = vol + sum
  }
  Sigma1 = sqrt((1/n)*vol)
  Sigma.V = Sigma1*sqrt(250)
  
  return(Sigma.V)
}

Sigma.V = Sigma.fun(u)






# We considered using the nlm-function in R to minimize our sigma function ...
# using iterlim = 3 to get three iterations and minimizing the sigma with respect to "mu".
# However, we found this solution wrong because the "mu" is constant, and we don't want to...
# change this to minimize sigma.






