get.Static.model <- function(return, IS.start = 1, IS.end = 1, OOS.start = 1, OOS.end = 1){
  IS.periods = IS.end-IS.start+1
  OOS.periods = OOS.end-OOS.start+1
  IS.omega.rest = NULL
  IS.omega.norest = NULL
  OOS.omega.rest = NULL
  OOS.omega.norest = NULL
  #---IS---
  IS.timeinterval = IS.start:IS.end
  if(length(IS.timeinterval)>1){
  cov.uncond <- cov(return[IS.timeinterval,])#sample covariance
  #Without restriction
  IS.omega.norest <- get.realized.optimal.portfolio.weights(as.matrix(cov.uncond))
  IS.omega.norest <- do.call("rbind", replicate(IS.periods, IS.omega.norest, simplify = FALSE))
  #With restriction
  IS.omega.rest <- get.realized.optimal.portfolio.weights.positive(cov.uncond)
  IS.omega.rest <- do.call("rbind", replicate(IS.periods, IS.omega.rest, simplify = FALSE))
  }
  #---OOS---
  OOS.timeinterval = OOS.start:OOS.end
  if(length(OOS.timeinterval)>1){
  #Without restriction
  OOS.omega.norest <- IS.omega.norest[nrow(IS.omega.rest),]
  OOS.omega.norest <- do.call("rbind", replicate(OOS.periods, OOS.omega.norest, simplify = FALSE))
  #With restriction
  OOS.omega.rest <- IS.omega.norest[nrow(IS.omega.rest),]
  OOS.omega.rest <- do.call("rbind", replicate(OOS.periods, OOS.omega.rest, simplify = FALSE))
  }
  
  return(list(omega.IS.norest = IS.omega.norest, omega.IS.rest = IS.omega.rest, omega.OOS.norest = OOS.omega.norest, omega.OOS.rest = OOS.omega.rest))
  }