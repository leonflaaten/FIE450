## TASK 3: BACKTESTING A MINIMUM VARIANCE STRATEGY

rm(list=ls()) ## clear environment
library(quadprog)

## Data preparation 

load("FIE450-Assignment-2-3.RData") # loading data

merge = merge(mkt, df, by = "Date") # merging the two data sets

for (i in 2:ncol(merge)) { # setting return observations to decimals
  for (j in 1:nrow(merge)) {
    merge[j, i] = merge[j, i] / 100
  }
}


##########


rolling.w.size <- 24 # setting rolling window size
periods <- nrow(merge)-rolling.w.size # rolling periods number
r <- rep(0, periods) # empty return vector

## Rebalance portfolio for each rolling window

for(i in 1:periods){
  
  # new data frame using our rolling window of 24 periods
  rolling.w <- merge[i:((rolling.w.size-1)+i),]
  
  # excluding non-trading stocks
  rolling.w <- rolling.w[apply(rolling.w, 2, function(R) !any(is.na(R)))] 
 
  # showing iteration progress of the for loop
  print(paste("Iteration: ", i, "of", periods)) 
 
  # creating data frame with the return observations for the next period
  next.period <- merge[rolling.w.size + i, c(colnames(rolling.w))]
  next.period <- next.period[, colSums(is.na(next.period)) == 0]  # assume that obs of next period cannot be NA
 
  # estimating the coefficients (alpha, beta, var.eps) using linear regression
  reg <- apply(rolling.w[ , -c(1,2)], 2, function (v) { 
    res <- lm(v ~ rolling.w$MKTRF3) 
    c(coefficients(res), var(residuals(res)))
  })
  rownames(reg) <- c("alpha", "beta", "var.eps") # change row names of reg
  
  # assigning values to the variables in the Single Index Model
  alpha <- reg[1, ] 
  beta <- reg[2, ] 
  var.eps <- reg[3, ] 
  mu.index <- mean(rolling.w$MKTRF3) # defining the average market return for the current period
  var.index <- var(rolling.w$MKTRF3) # defining the market variance for the current period
  mu <- beta * mu.index # defining individual stock's expected return for the current period
  Sigma <- var.index * (as.matrix(beta) %*% beta) # defining the standard deviation for the portfolio
  diag(Sigma) <- diag(Sigma) + var.eps # adding the variance of the residuals
  A <- t(array(1 , dim = c(1,length(mu)))) # defining constraints
  b0 <- 1 # the sum of weights will be equal 1
  d = rep(0, length(mu))
  res <- solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 1) # solving QP
  omega <- res$solution # assigning optimal solution weights to a vector
  r[i] <- sum(omega*next.period[ , -(1:2)]) # computing sum of weighted returns forward looking
  
}

r

## computing annualized mean
mu <- mean(r)*12
mu

## computing annualized sharpe ratio
sd = sd(r)*sqrt(12)

sr <- mu / sd
sr


