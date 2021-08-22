library(R.matlab)
library(tidyverse)
library(vars)
#VARMA
library(MTS)

#calculate weights
list.realCov.Date <- readMat("Cluster_30D_realcov_cleaner_liquiditysorting.mat")

#realized optimal portfolio weights
vec.unity <- function(lenghts){
  return(rep(1, lenghts))
}
optimalweights <- data.frame()
for(i in 1:3272){
  cov <- list.realCov.Date$R[,,i]
  unity.vector <- t(vec.unity(ncol(cov)))
  optimalweights_i <- solve(unity.vector %*% solve(cov) %*% t(unity.vector))[[1]] * solve(cov) %*% t(unity.vector)
  optimalweights <- rbind(optimalweights, t(optimalweights_i))
}
optimalweights <- cbind(list.realCov.Date$DATE.R[1,], optimalweights)
colnames(optimalweights)[1] <- "Date"

#---MA and AR model---
{
  #Standard AR model
  obs <- (optimalweights[,2])
  N <- length(obs)
  y <- obs[3:N]
  
  x <- cbind(-obs[2:(N-1)], -obs[1:(N-2)])
  solve(t(x) %*% x, t(x) %*% y)
}
#ARMA with 2 pass estimation
{
  #step 1: fit AR model
  obs <- (optimalweights[,2])
  N <- length(obs)
  y <- matrix(obs[2:(N)])
  
  x <- cbind(-obs[1:(N-1)])
  
  beta1 <- solve(t(x)%*%x)%*%t(x)%*%y
  
  error_ar <- y - x %*% beta1
  #step 2: fit MA model on error
  N <- length(error_ar)
  y <- matrix(error_ar[2:(N)])
  x <- cbind(error_ar[1:(N-1)])
  beta2 <- solve(t(x)%*%x)%*%t(x)%*%y
  
}
#Standard ARMA model
{
  coefficients <- data.frame(ar1 = NA, ma1 = NA, ma2= NA , intercept = NA)
  coefficients <- coefficients %>% filter(is.na(ar1) == FALSE)
  Model_ARMA <- c(1, 0, 1)

  for(i in 2:ncol(optimalweights)){
    y <- ts(optimalweights[i])
    msft_ma <- arima(y, order = Model_ARMA, method ="ML")
    coeff_i <- as.data.frame(t(msft_ma$coef))
    coefficients <- rbind(coefficients, coeff_i)
  }
  coefficients$B <- -coefficients$ma1
  coefficients$A <- coefficients$ar1 + coefficients$ma1
  mean(coefficients$A)
  min(coefficients$A)
  max(coefficients$A)
  
  mean(coefficients$B)
  min(coefficients$B)
  max(coefficients$B)
  
  mean(coefficients$ar1)
  min(coefficients$ar1)
  max(coefficients$ar1)
  
  y <- ts(optimalweights[10])
  acf(y)
  pacf(y)
  
  Model_ARMA <- c(1, 0, 2) # MA :  c(0, 0, 1) AR: c(1, 0, 0)
  #fit model to data
  msft_ma <- arima(y, order = Model_ARMA, method ="ML")
  msft_ma
  msft_ma$coef
  
  ts.plot(y)
  AR_fit <- y - residuals(msft_ma)
  points(AR_fit, type = "l", col = 2, lty = 2)
  
}
y <- as.data.frame(optimalweights[,10])
y <- as.ts(y)
plot.ts(y)
acf(y)
pacf(y)

Model_ARMA <- c(1, 0, 0) # MA :  c(0, 0, 1) AR: c(1, 0, 0)
#fit model to data
msft_ma <- arima(y, order = Model_ARMA)
msft_ma

residuals <- residuals(msft_ma)
msft_fitted <- y - residuals
ts.plot(y)
points(msft_fitted, type = "l", col = 2, lty = 2)


#Phillips-Perron stationary test
pptab <- NULL 
for(i in 2:ncol(optimalweights)){
  pp <- PP.test(optimalweights[,i])
  pptab <- rbind(pptab, pp$p.value) 
}
#->All Variables are stationary

var <- optimalweights[-1]
var <- var[1:10]
var <- as.ts(var)
plot.ts(var)

lagselect <- VARselect(var, lag.max = 15, type = "const")
lagselect$selection

Model1 <- VAR(var, p=1, type = "none")

Model2 <- VARMA(var, p=1, q=1, include.mean = FALSE)
summary(Model2)

var.result <- Model1$datamat

#forecast
forecast <- predict(Model1, n.ahead = 10, ci=0.95)
fanchart(forecast, names = "V5")


# Arch1 <- arch.test(Model1, lags.multi = 15, multivariate.only = TRUE)
# Arch1
# 
# Norm1 <- normality.test(Model1, multivariate.only = TRUE)
# Norm1
forecast <- predict(Model1, n.ahead = 1, ci = 0.95)


#Plot
ggplot(optimalweights) + geom_point(aes(x = Date, y = V1))

attach(optimalweights)
ts.v1 <- ts(V1)
ts.v2 <- ts(V2)

OLS <- lm(ts.v1 ~ ts.v2)
summary(OLS)

#Determine the persistance of the model
acf(ts.v1)
pacf(ts.v1)
acf(ts.v2)
pacf(ts.v2)

ts.bv <- cbind(ts.v1, ts.v2)
colnames(ts.bv) <- c("omega","v")

lagselect <- VARselect(ts.bv, lag.max = 10, type = "const")
lagselect$selection

#Building VAR
Model1 <- VAR(ts.bv, p=1, type = "const", season = NULL, exog = NULL)
Model1


