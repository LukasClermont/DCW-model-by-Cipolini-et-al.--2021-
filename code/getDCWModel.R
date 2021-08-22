get.DCW.model <- function(omega.real.norest, omega.real.rest , IS.start = 1, IS.end = 1, OOS.start = 1, OOS.end = 1){
  
  IS.periods = IS.end-IS.start
  OOS.periods = OOS.end-OOS.start+1
  IS.timeinterval = IS.start:IS.end
  OOS.timeinterval = OOS.start:OOS.end
  IS.omega.rest = NULL
  IS.omega.norest = NULL
  OOS.omega.rest = NULL
  OOS.omega.norest = NULL
  
  
  coefficients <- data.frame(ar1 = NA, ma1 = NA , intercept = NA)
  Modeltype_ARMA <- c(1, 0, 1)
  
  #---Without restriction---  
  for(i in 1:ncol(omega.real.norest)){
    #Compute model
    y <- omega.real.norest[IS.timeinterval,i]
    arma11 <- arima(y, order = Modeltype_ARMA, method ="ML")
    coeff_i <- as.data.frame(t(arma11$coef))
    coefficients <- rbind(coefficients, coeff_i)
    if(length(IS.timeinterval)>1){
    #In-Sample
    IS.epsilon.t <- as.matrix(residuals(arma11))
    IS.omega.t <- y - IS.epsilon.t
    IS.omega.norest <- rbind(IS.omega.norest, as.data.frame(t(IS.omega.t)))
    }
    if(length(OOS.timeinterval)>1){
    #Out-Of-Sample
    OOS.omega.t <- predict(arma11, n.ahead = OOS.periods) # newdata = y.new
    OOS.omega.t <- OOS.omega.t[[1]]
    OOS.omega.norest <- rbind(OOS.omega.norest, t(OOS.omega.t))
    }
  }

  #---With restriction---  
  for(i in 1:ncol(omega.real.rest)){
    #Compute model
    i = 1
    y <- omega.real.rest[IS.timeinterval,i]
    arma11 <- arima(y, order = Modeltype_ARMA, method ="ML")
    coeff_i <- as.data.frame(t(arma11$coef))
    coefficients <- rbind(coefficients, coeff_i)
    if(length(IS.timeinterval)>1){
      #In-Sample
      IS.epsilon.t <- as.matrix(residuals(arma11))
      IS.omega.t <- y - IS.epsilon.t
      IS.omega.rest <- rbind(IS.omega.rest, as.data.frame(t(IS.omega.t)))
    }
    if(length(OOS.timeinterval)>1){
      #Out-Of-Sample
      OOS.omega.t <- predict(arma11, n.ahead = OOS.periods) # newdata = y.new
      OOS.omega.t <- OOS.omega.t[[1]]
      OOS.omega.rest <- rbind(OOS.omega.rest, t(OOS.omega.t))
    }
  }
  if(length(IS.timeinterval)>1){
    IS.omega.norest <- t(IS.omega.norest)
    IS.omega.rest <- t(IS.omega.rest)
  }
  if(length(OOS.timeinterval)>1){
    OOS.omega.norest <- t(OOS.omega.norest)
    OOS.omega.rest <- t(OOS.omega.rest)
  }
  
  
  
  return(list(omega.IS.norest = IS.omega.norest, omega.IS.rest = IS.omega.rest, omega.OOS.norest = OOS.omega.norest, omega.OOS.rest = OOS.omega.rest))
}