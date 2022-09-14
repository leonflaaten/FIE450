rm(list=ls())

# Reading and cleaning the data
NHY_daily = get(load("NHY.OL-daily.RData"))
NHY_daily[,1] = as.Date(NHY_daily[,1],format = "%d.%m.%y")
NHY_daily = NHY_daily[(NHY_daily$Date >= "2020-01-01"), ]

# Compute log returns
NHY_daily$r = c(NA,diff(log(NHY_daily$p)))

# remove NA values
r = na.omit(NHY_daily$r)


# Compute the GARCH(2,2) variance

# r : vector of returns
# Output is a vector of GARCH variances
garch.var = function(r,omega,alpha1,alpha2,beta1,beta2) {
  sigma2 = c(r[1]^2,r[2]^2)
  
  for (i in 3:length(r)) {
    sigma2 = c(sigma2, omega + alpha1*r[i-1]^2+ alpha2*r[i]^2 + beta1*sigma2[i-1] +beta2*sigma2[i-2])
  }
  return(sigma2)
}


# Log-likelihood function for GARCH volatility
# Par : vector of GARCH parameters
# Ouput is the negative of the log-likelihood.
garch.ll.fun = function(par,r) {
  omega = par[1]
  alpha1 = par[2]
  alpha2 = par[3]
  beta1 = par[4]
  beta2 = par[5]
  sigma2 = garch.var(r,omega,alpha1,alpha2,beta1,beta2)
  r = r[-1]
  sigma2 = sigma2[-length(sigma2)]
  ll = sum(-log(sigma2)-r^2/sigma2)
  return(-ll)
}

## Optimizing the GARCH(2,2) parameters.
res = nlminb(c(0.001,0.2,0.2,0.2,0.2),garch.ll.fun, lower=1e-6,upper=1-(1e-6),r=r)
res


# Obtain GARCH parameters:
omega = res$par[1]
alpha1 = res$par[2]
alpha2 =  res$par[3]
beta1 = res$par[4]
beta2 = res$par[5]


gamma = 1 - alpha1 - beta1 - alpha2 - beta2


#Annualized GARCH volatility
VL = omega/gamma
VL = sqrt(VL*250)
VL
