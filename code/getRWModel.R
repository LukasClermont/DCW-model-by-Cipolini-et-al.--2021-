get.RW.model <- function(omega.real.norest, omega.real.rest , IS.start = 1, IS.end = 1, OOS.start = 1, OOS.end = 1){
  IS.periods = IS.end-IS.start
  OOS.periods = OOS.end-OOS.start+1
  IS.timeinterval = IS.start:IS.end
  OOS.timeinterval = OOS.start:OOS.end
  IS.omega.rest = NULL
  IS.omega.norest = NULL
  OOS.omega.rest = NULL
  OOS.omega.norest = NULL
  #---IS---
  if(length(IS.timeinterval)>1){
    #without restiction
    IS.realomega.norest <- omega.real.norest[IS.timeinterval,]
    IS.omega.norest <- rbind(IS.realomega.norest[1,], IS.realomega.norest)
    IS.omega.norest <- IS.omega.norest[-nrow(IS.omega.norest),]

    #with restiction
    IS.realomega.rest <- omega.real.rest[IS.timeinterval,]
    IS.omega.rest <- rbind(IS.realomega.rest[1,], IS.realomega.rest)
    IS.omega.rest <- IS.omega.rest[-nrow(IS.omega.rest),]

  }
  #---OOS---
  if(length(OOS.timeinterval)>1){
    #without restriction
    OOS.omega.norest <- omega.real.norest[IS.end,]
    OOS.omega.norest <- do.call("rbind", replicate(OOS.periods, OOS.omega.norest, simplify = FALSE))
    #with restriction
    OOS.omega.rest <- omega.real.rest[IS.end,]
    OOS.omega.rest <- do.call("rbind", replicate(OOS.periods, OOS.omega.rest, simplify = FALSE))
    
  }
    

  return(list(omega.IS.norest = IS.omega.norest, omega.IS.rest = IS.omega.rest, omega.OOS.norest = OOS.omega.norest, omega.OOS.rest = OOS.omega.rest))
}