get.data.HAR <- function(interval, i ,list.realCov.Date){
  volatw = NULL 
  volatm = NULL 
  volatd = NULL
  s_i <- list.realCov.Date[i,i,interval]
  for (t in 23:length(s_i)){
    volatd[t] = s_i[(t-1)]
    volatw[t] = sum(s_i[(t-5):(t-1)],na.rm = T)/5
    volatm[t] = sum(s_i[(t-22):(t-1)],na.rm = T)/22 
  }
  data.HAR <- as.data.frame(cbind(s_i, volatd, volatw, volatm))
  return(data.HAR[-c(1:23),])
}
get.HAR.model <- function(cov , IS.start = 1, IS.end = 1, OOS.start = 1, OOS.end = 1){
  IS.periods = IS.end-IS.start
  OOS.periods = OOS.end-OOS.start
  IS.timeinterval = IS.start:IS.end
  OOS.timeinterval = OOS.start:OOS.end
  OOS.HAR = NULL
  IS.HAR = NULL

  IS.omega.rest = NULL
  IS.omega.norest = NULL
  OOS.omega.rest = NULL
  OOS.omega.norest = NULL
  
  for(i in 1:nrow(cov)){
    #In-Sample
    if(length(IS.timeinterval)>1){
    data.IS <- get.data.HAR(IS.timeinterval, i ,cov)
    lm.VT_i = lm(s_i~volatd+volatw+volatm, data = data.IS)
    IS.HAR.i <- predict(lm.VT_i, newdata = data.IS)
    IS.HAR <- rbind(IS.HAR, t(IS.HAR.i))
    }
    #OOS-Sample
    if(length(OOS.timeinterval)>1){
    data.OOS <- get.data.HAR(OOS.timeinterval, i ,cov)
    OOS.HAR.i <- predict(lm.VT_i, newdata = data.OOS)
    OOS.HAR <- rbind(OOS.HAR, t(OOS.HAR.i))
    }
  }
 
  
  #---get Weights
  #OOS
  if(length(OOS.timeinterval)>1){
    OOS.HAR <- t(OOS.HAR)
    for(i in 1:nrow(OOS.HAR)){
      cov.HAR <- diag(OOS.HAR[i,])
      
      #OOS.omega.rest.i <- get.realized.optimal.portfolio.weights.positive(cov.HAR)
      OOS.omega.norest.i <- get.realized.optimal.portfolio.weights(cov.HAR)
      
      #OOS.omega.rest <- cbind(OOS.omega.rest, OOS.omega.rest.i)
      OOS.omega.norest <- rbind(OOS.omega.norest, OOS.omega.norest.i)
    }
  }
  if(length(IS.timeinterval)>1){
  #In-Sample
    IS.HAR <- t(IS.HAR)
    for(i in 1:nrow(IS.HAR)){
      cov.HAR <- diag(IS.HAR[i,])
      
      #IS.omega.rest.i <- get.realized.optimal.portfolio.weights.positive(cov.HAR)
      IS.omega.norest.i <- get.realized.optimal.portfolio.weights(cov.HAR)
      
      #IS.omega.rest <- rbind(IS.omega.rest, IS.omega.rest.i)
      IS.omega.norest <- rbind(IS.omega.norest, IS.omega.norest.i)
    }
  }
  
  return(list(omega.IS.norest = IS.omega.norest, omega.IS.rest = IS.omega.rest, omega.OOS.norest = OOS.omega.norest, omega.OOS.rest = OOS.omega.rest))
}
