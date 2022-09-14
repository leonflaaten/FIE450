rm(list=ls())

#loading the data and renaming them, because both name is df
NHY_daily = get(load("NHY.OL-daily.RData"))
NHY_weekly = get(load("NHY.OL-weekly.RData"))

#Remove df
rm(df)

#Using log return
NHY_daily$r = c(NA,diff(log(NHY_daily$p)))
NHY_weekly$r = c(NA,diff(log(NHY_weekly$p)))


#Computing annualized mean
mu.d = mean(NHY_daily$r,na.rm=TRUE)*250
mu.w = mean(NHY_weekly$r,na.rm=TRUE)*52


#Compute Standard Error
r.d = na.omit(NHY_daily$r)
r.w = na.omit(NHY_weekly$r)
n.d = length(r.d)
n.w = length(r.w)

#Daily
SE.d = sd(r.d)/sqrt(n.d)*250


#Weekly
SE.w = sd(r.w)/sqrt(n.w)*52


#Putting results in a table
results = data.frame(Annualized_Exp_R = c(mu.d,mu.w),
                     St_Error = c(SE.d,SE.w))


row.names(results) = c('Daily', 'Weekly')
results


# Which sample produce more precise estimates of expected returns?

#Comment:
# The frequency of the sampling does not impact the precision of the estimates of expected returns.
# To get more precise estimates we need to go further back in time and increase the sampling.
# When it comes to standard deviation the precision is increased with higher frequency in the sampling.



