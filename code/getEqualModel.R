get.Equal.model <- function(return, IS.start = 1, IS.end = 1, OOS.start = 1, OOS.end = 1){
  IS.periods = IS.end-IS.start +1
  OOS.periods = OOS.end-OOS.start +1
  IS.omega <- as.data.frame( t(rep(1/ncol(return), ncol(return))))
  IS.omega <- do.call("rbind", replicate(IS.periods, IS.omega, simplify = FALSE))
  OOS.omega <- as.data.frame( t(rep(1/ncol(return), ncol(return))))
  OOS.omega <- do.call("rbind", replicate(OOS.periods, OOS.omega, simplify = FALSE))
  return(list(omega.IS.norest = IS.omega, omega.IS.rest = IS.omega, omega.OOS.norest = OOS.omega, omega.OOS.rest = OOS.omega))
}
