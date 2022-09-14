rm(list=ls())
library(quadprog)
library(Matrix)
load("FIE450-Assignment-2-2.RData")


# In this task we find the expected excess return and not the expected return.
# We assume we are going to use Excess return as expected return since this is used in the lectures
# The expected return is excess return plus the risk free rate.


rf = 0.02

# Subtract risk-free interest rate from the expected return.
# To get excess returns.
mu = mu - rf



# Task 2.1 // Unconstrained Frontier

mu.p.vec = seq(0,0.6,by=0.01)     #range from 0 to 0.6 with increments of 0.01
sigma.p.vec = c()                 # empty vector to add the sigmas in the for loop to.
A = t(rbind(1,mu))                # matrix defining our constraints
d = rep(0,length(mu))             # create a vector with zeros


for (i in 1:length(mu.p.vec)) {
  mu.star = mu.p.vec[i]            # the expected return we will find the optimal weights for
  b0 = c(1,mu.star)                # the return will be equal mu.star
  
  # Solve.QP finds the stocks weights which minimize our portfolio variance given mu.star
  res = solve.QP(Dmat=Sigma,dvec=d,Amat=A,bvec=b0,meq=2)     
  omega = res$solution              # optimal solution weights 
  # computing volatility and adding to vector.
  sigma.p.vec = c(sigma.p.vec,sqrt(t(omega)%*%Sigma%*%as.matrix(omega)))     
}


# Plot the efficient frontier.
plot(sigma.p.vec, mu.p.vec, type = "l",col="blue", xlim = c(0, max(sigma.p.vec)),
     xlab = "Volatility", ylab = "Excess return")


# Adding the expected returns and
# the volatilities of the stocks to the plot

vol = c()
for (i in 1:ncol(Sigma)) {        # we have the expected returns 
  vol = c(vol,sqrt(Sigma[i,i]))   # but need to compute the sigma.
}

# Adding the expected returns and the volatilities of the stocks to the plot.
points(vol,mu,pch=19)






# Task 2.2 // Constrained Frontier 

# Since a portfolio with no shortselling be 100% in the stock
# with the lowest return at the minimum and at the maximum be 100%
# in the stock with the highest return.
# the start value for the efficient frontier will be the minimum of mu
# and the end will be the maximum of mu

mu.p.vec2 = seq(min(mu),max(mu),by=0.01)
sigma.p.vec2 = c()
A = t(rbind(1,mu,diag(1,length(mu))))     # matrix defining our constraints

for (i in 1:length(mu.p.vec2)) {
  d = rep(0,length(mu))                   # creating a vector of zeros
  mu.star = mu.p.vec2[i]
  # the return will be equal to mu.star and no shorting.
  b0 = c(1,mu.star,rep(0,length(mu)))     
  res2 = solve.QP(Dmat=Sigma,dvec=d,Amat=A,bvec=b0,meq=2)   # solving for optimal weights
  omega2 = res2$solution
  sigma.p.vec2 = c(sigma.p.vec2,sqrt(t(omega2)%*%Sigma%*%as.matrix(omega2)))
}

# Adding the constrained frontier to the previous plot.
lines(sigma.p.vec2, mu.p.vec2,col="red")





# Task 2.3  // Computing the MVP portfolio when short-selling allowed.

mu.star = 0.01              # starting value 
A = t(rbind(1,mu))          # matrix defining our constraints
d = rep(0,length(mu))       # creating a vector of zeros
b0 = c(1,mu.star)          
# Solve the model where we minimize volatility 
#where return will be greater or equal mu.star. 
res3 = solve.QP(Dmat=Sigma,dvec=d,Amat=A,bvec=b0,meq=1)

omega3 =res3$solution           # MVP weights.
mu.min = t(omega3) %*% as.matrix(mu)    # computing the expected excess return of the portfolio.
mu.min = mu.min[1]             #from dataframe to value

sig.min = sqrt(t(omega3)%*%Sigma%*%as.matrix(omega3))   # computing the volatility of the portfolio.
sig.min = sig.min[1]            #from dataframe to value

sr.min = (mu.min/sig.min)       # compute Sharpe ratio

# Adding the MVP to the plot.
points(sig.min,mu.min,col="orange",pch=19)


# Results task 2.3
mu.min
sig.min
sr.min




# Task 2.4 // Tangent Portfolio when short-selling is allowed.

mu.star = 0.1            #starting value for mu.   
# vector with constraint. The sum  of weights does not need to be 1.
b0 = c(mu.star)          
d = rep(0,length(mu))     # creating a vector of zeros
A = t(rbind(mu))        # matrix defining our constraints


# Solve the model where we find a portfolio on the
# Capital Allocation Line with a target return of mu.star
res4 = solve.QP(Dmat=Sigma,dvec=d,Amat=A,bvec=b0,meq=1)

w = res4$solution   ## w is a portfolio on the capital allocation line with a target return of mu.star

# To find the tangency portfolio we rescale the w-vector so the weights sum up to 1.
omega.tan = w/sum(w)
omega.tan


# computing the expected excess return of the tangency portfolio.
mu.tan = t(omega.tan) %*% as.matrix(mu)
mu.tan = mu.tan[1]     #from dataframe to value

# computing the volatility of the tangency portfolio.
sig.tan = sqrt(t(omega.tan)%*%Sigma%*%omega.tan)
sig.tan = sig.tan[1]


# Sharpe ratio of the tangency portfolio
sr.tan = mu.tan/sig.tan


# Results task 2.4
mu.tan
sig.tan
sr.tan


points(sig.tan,mu.tan,col="green",pch=19)
#abline(0,mu.tan/sig.tan,lwd=2,col="darkred",lty=2)
legend("topleft",
       legend = c("Short", "No short", "Stocks","Tangency","MVP"),
       col= c("blue", "red", "black","green","orange"),
       pch = 19,
       bty = "n")




# Task 2.5
#Optimal portfolio with 30% volatility and Short selling is allowed

# Function that computes the optimal portfolio
# with an expected return of mu.star
obj.fn = function(mu.star,sigma.star) {
  b0 = c(1,mu.star)     
  d = rep(0,length(mu))
  A = t(rbind(1,mu))
  res = solve.QP(Dmat=Sigma,dvec=d,Amat=A,bvec=b0,meq=2)
  omega5 = res$solution
  sig5 = sqrt(t(omega5) %*% Sigma %*% as.matrix(omega5))
  
  # return the squared difference of the optimal portfolio's volatility
  # and sigma.star
  diff = (sig5-sigma.star)^2                    
  return(diff)
}

sigma.star = 0.3      # 30% volatility

# nlminb finds the corresponding expected portfolio return
res5 = nlminb(0.1,obj.fn,lower=0,upper=1,sigma.star=sigma.star)
mu.30 = res5$par


sr.30 = mu.30 / sigma.star


# Computing the weights / Omega.30

b0 = c(1,mu.30)     
d = rep(0,length(mu))
A = t(rbind(1,mu))
resO = solve.QP(Dmat=Sigma,dvec=d,Amat=A,bvec=b0,meq=2)
omega.30 = resO$solution 


# Results task 2.5
mu.30
omega.30
sr.30





# Task 2.6 // Optimal Portfolio with a target return of 20%

n = length(mu)              # number of stocks
A = t(rbind(1,mu,diag(1,n),diag(-1,n)))     # Matrix defining out 
d = rep(0,n)              # vector of n zeros

# the sum of weights will be 1, target return is 20% and a constraint for maximum weight of a stock equal 15%.
b0 = c(1,0.2,rep(0,n),rep(-0.15,n))     

# Solving the model
res6 = solve.QP(Dmat=Sigma,dvec=d,Amat=A,bvec=b0,meq=2)

# optimal weights
omega.20 = res6$solution 

# Compute the excess return of the portfolio
mu.20 = t(omega.20) %*% as.matrix(mu)
mu.20 = mu.20[1]     #from dataframe to value

# Compute the volatility of the portfolio.
sig.20 = sqrt(t(omega.20) %*% Sigma %*% as.matrix(omega.20))
sig.20 = sig.20[1]   # from dataframe to value


# Sharpe ratio
sr.20 = mu.20/sig.20

# Results task 2.6
mu.20
sr.20
omega.20

