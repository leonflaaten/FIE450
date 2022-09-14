rm(list=ls()) ## clear environment

## load the data
load("FIE450-Assignment-2-1.RData")

## function to compute simple return
fn = function(v) {
  c(v[-1]/v[-length(v)] - 1, NA)
}

## computing stock simple return
df$R = unlist(tapply(df$AdjLast, df$SecurityId, fn))

## cleaning data making sure returns do not span 
## a time period longer than a month
df = df[order(df$SecurityId, df$Date), ]
df$delta.t = unlist(tapply(df$Date, list(df$SecurityId), 
                           function(v) c(as.numeric(diff(v)), NA)))

df = df[!is.na(df$delta.t), ]
df = df[df$delta.t <= 31, ]
df$delta.t = NULL

## reshaping the stock's data from long to wide format
stocks.wide = reshape(df[,c("Date","SecurityId","R")],
                      v.names = "R",idvar = "Date",timevar="SecurityId",direction="wide")

## ordering by date
stocks.wide = stocks.wide[order(stocks.wide$Date),]

## at least 50% non-NA return observations during the entire sample period
atLeast = 0.5
n = length(stocks.wide$Date)
stocks.wide = stocks.wide[,colSums(!is.na(stocks.wide))/n >= atLeast]

# Since we have  forward looking returns, we use this code.
# To remove stocks with no return obs in november
stocks.wide <- stocks.wide[, !is.na(stocks.wide[nrow(stocks.wide), ])]

## obs starting observation equal to stocks df starting observations
obx = obx[(obx$Date >= df$Date[1]), ]

## computing simple return for obx
obx$RM <- c(obx$Close[-1]/obx$Close[-length(obx$Close)] - 1, NA)
obx = obx[,c("Date","RM")] ## keeping date and return market

## merging obx dataframe and the stocks.wide data frame by the dates
df = merge(obx,stocks.wide,by="Date")

## regressing function for Single Index Model
reg = apply(df[,-1],2,function(v) {
  res = lm(v~df$RM)
  c(coefficients(res),var(residuals(res)))
})

rownames(reg) <- c("alpha", "beta", "var.eps") ## changing rownames

## obtaining annual values from the Single Index Model
alpha = reg[1,] * 12
beta = reg[2,]
var.eps = reg[3,]*12
mu.index = mean(df$RM,na.rm=TRUE)*12 ## define the average return of obx
var.index = var(df$RM,na.rm=TRUE)*12 ## define the variance in obx

# 1. the annualized expected stock return [mu]
mu = beta * mu.index
mu

# 2 Calculating the annualized covariance matrix  [Sigma]
Sigma = var.index * (as.matrix(beta)%*% beta) # Std dev for the portfolio
diag(Sigma) = diag(Sigma)+var.eps #adding variance of the residuals
Sigma
