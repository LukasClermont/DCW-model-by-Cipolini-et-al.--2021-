#Variance
get.portfolio.variance <- function(w, list.cov){
  w = as.matrix(w)
  T = nrow(w)
  summe = 0
  for(t in 1:T){
    S_t = list.cov[,,t]
    summe_t = t(w[t,]) %*% S_t %*% w[t,]
    summe = summe + summe_t
  }
  port.var = 1/T * summe
  return(port.var[1])
}
#Return
get.portfolio.mean.return <- function(w, R){
  w = as.matrix(w)
  R = as.matrix(R)
  N = ncol(R)
  T = nrow(R)
  port.return = 1/(N*T) * sum(t(w) %*% R)
  return(port.return[1])
}

#Sharp Ratio
get.portfolio.mean.SR <- function(w, R, list.cov){
  SR = get.portfolio.mean.return(w, R) / sqrt(get.portfolio.variance(w, list.cov))
  return(SR)
}