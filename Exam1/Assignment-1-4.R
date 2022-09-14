
rm(list=ls())

simulate.paths.fast = function(S0,rf,sigma,dt,T,n) {
  t = seq(dt,T,by=dt)       # sequence of time T with time step dt
  m = length(t)             #length of t (sequence of time)
  e = matrix(exp((rf-0.5*sigma^2)*dt+sigma*sqrt(dt)*rnorm(m*n)),m,n,byrow=FALSE)
  # create a matrix for the exponential factor.
  S = S0*apply(e,2,cumprod)   # compute the cumulative product from the matrix, and apply to the matrix
  S = rbind(S0,S)         # include S0 in the final matrix.
  return(S)
}

payoffs = function(S,K,b,rf,f) {
  I = apply(b>S,2,all)       #barrier
  # if price is below barrier once then the value is 1
  X = I*pmax(K-S[nrow(S), ],0)*f
  # multiply I with or payoff function.
  X0 = X*exp(-rf*T)  #discount the payoff
  return(X0)
}

# Input Variables
K = 1000     #strike price
b = 1060   # barrier
T = 3/52    # maturity (in years)
SO = 1050        #Stock 
Sigma = 0.32    # given volatility
rf = 0.008
dt = 1/52     # time grid
n = 10000     #simulate n = 10 000 price paths
f = 1          

set.seed(1)
S = simulate.paths.fast(SO,rf,Sigma,dt,T,n)

X = payoffs(S,K,b,rf,f)
V = mean(X)



## 99% confidence interval

n = length(X)
n
SE = sd(X)/sqrt(n)  
SE

alpha = 0.99
z = -qnorm((1-alpha)/2)

ci = c(V-z*SE,V+z*SE)





## Antithetic variates

simulate.paths.fast.as = function(S0,rf,sigma,dt,T,n) {
  t = seq(dt, T, by=dt)              #sequence of time with time step dt
  m = length(t)                      #length of t
  z = rnorm(n*m)                     #creating n*m random variables that are normally distributed.
  z.as = -z                          #antithetic counterpart of variable z
  Z = matrix(c(z, z.as),m,n*2)       #matrix of the variables z and z.as
  e = exp((rf-0.5*sigma^2)*dt+sigma*sqrt(dt)*Z)  #matrix of exponential factor
  S = apply(e,2,cumprod)             #compute cumulative product from the matrix, and apply to the matrix
  S = S*S0                           # to get simulated values
  S = rbind(S0,S)                    # add Simulated value to the final matrix
  return(S)
}

n = 5000     # 10 000 / 2 to adjust for the pairing
S.as = simulate.paths.fast.as(SO,rf,Sigma,dt,T,n=n)

# compute payoffs
X0.as = payoffs(S.as,K,b,rf,f=1)


# Monte Carlo estimator.
V.as = mean(X0.as)
V.as


# Standard error of Monte-Carlo estimator
X.pairs = (X0.as[1:n] + X0.as[(n+1):(2*n)])/2
SE.as = sd(X.pairs)/sqrt(n)

alpha = 0.99
z = -qnorm((1-alpha)/2)

# 99% confidence interval of Monte-Carlo estimator
ci.as = c(V.as-z*SE.as,V.as+z*SE.as)


# Does antithetic sampling improve the accuracy of the estimator? 

rho = cor(X0.as[1:n], X0.as[(n + 1):(2*n)])


#Does antithetic sampling improve the accuracy?

print((rho<0))

# The value is less than zero, therefore we observe that antithetic sampling improve the accuracy.


#To summarize, we put all the results in a table
mont_lst = list(c(V-z*SE,V+z*SE))
mont_lst
as_lst = list(c(V.as-z*SE.as,V.as+z*SE.as))

results = data.frame(Results = c(V, 0, V.as, 0))
results$Results[2] = c(mont_lst)
results$Results[4] = c(as_lst)
row.names(results) = c("V", "ci", "V.as", "ci.as")

results
