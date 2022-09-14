rm(list=ls())


rf = 0.0080

T = (as.numeric(as.Date('2022-02-18') - as.Date('2022-01-28')))/365


######## Computing the implied volatility of the put options ########

SP = 1050.49       # Value of the OBX index 28.01.2022
KP = 1000          #Strike price of the Put Option
P.market = 12.25


# This function will compute the price of the put, using the Black-Scholes Model
Put = function(SO,sigma,K,rf,T) {
  d1 = (log(SO/K) + (rf + sigma^2/2)*T)/(sigma*sqrt(T))
  d2= d1 - sigma*sqrt(T)
  P = K*exp(-rf*T)*pnorm(-d2) - SO*pnorm(-d1)
  return(P)
}

# Defining the objective function, this will find the squared error 
# between the market price and our model
obj.funPut = function(sigma, P.market, SO, K, rf, T) {
  P.model = Put(SO, sigma, K, rf, T)
  eps = (P.model-P.market)^2
  return(eps)
}

#Then we want to use the optimizer to find implied volatility
res_Put = nlm(obj.funPut, p=0.5, P.market=P.market, SO=SP, K=KP, rf=rf, T=T)
sigp = res_Put$estimate
sigp



######## Computing the implied volatility for the call option #########

SC = 1050.49      # Value of the OBX index 28.01.2022
KC = 1080         # Strike price


# This function will compute the price of the Call, using the Black-Scholes Model
call = function(SO,sigma,K,rf,T) {
  d1 = (log(SO/K) + (rf + sigma^2/2)*T)/(sigma*sqrt(T))
  d2= d1 - sigma*sqrt(T)
  C = SO*pnorm(d1)-exp(-rf*T)*K*pnorm(d2)
  return(C)
}

C.market = 10.75


# Defining the objective function 
obj.funCall = function(sigma, C.market, SO, K, rf, T) {
  C.model = call(SO, sigma, K, rf, T)
  eps = (C.model-C.market)^2
  return(eps)
}

# Optimizer
res_Call = nlm(obj.funCall, p=0.5, C.market=C.market, SO=SC, K=KC, rf=rf, T=T)
sigc = res_Call$estimate
sigc


######## Computing the implied volatility with 100% moneyness ########

# Here we made a simple function to compute the interpolated volatility.


# The function is runned with the OBX-price as x-values 
# Implied volatility for both put and call as y-values
# And set the x3 (Strike price) to 1050 to get 100% moneyness 

sigcal = function(x1,x2,y1,y2,x3) {
  m = (y2-y1)/(x2-x1)
  b = y1 - m*x1
  y3 = m*x3 + b
  return(y3)
}

sig = sigcal(KC,KP,sigc,sigp,1050.49)
sig

# All results in a table 
results = data.frame(Volatility = c(sigp, sigc, sig))
row.names(results) = c('sigp', 'sigc', 'sig')
results

## To summarize, the implied volatility for the put and the call is 0.31985 and 
## 0.219063. By using a simple linear interpolation formula we find the 
## Interpolated volatility with 100% moneyness to be 0.25624.




