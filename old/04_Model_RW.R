results.RW <- data.frame()

for(i in 1:intervals){
  model.rw <- get.RW.model(realoptweights, timeinterval[i]:timeinterval[i+timelag], t_pred)
  
  #Return
  predictions <- model.rw$prediction
  predictions.return.rw <- data.frame()
  for(t in 1:nrow(predictions)){
    predictions.return.rw[t,1] <- get.portfolio.variance(as.matrix(predictions[t,]), as.matrix(list.realCov.Date$R[,,timeinterval[i+timelag] + t]))
    predictions.return.rw[t,2] <- get.portfolio.mean.return(as.matrix(predictions[t,]), as.matrix(return[timeinterval[i+timelag] + t,]))
  }
  results.RW <- rbind(results.RW,apply(predictions.return.rw, 2, mean))
}
colnames(results.RW) <- c("Port.Variance.RW", "Port.Return.RW")


