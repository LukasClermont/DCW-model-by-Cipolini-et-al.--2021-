results.DCW <- data.frame()

for(i in 1:intervals){
  model.dcw <- get.DCW.model(realoptweights, timeinterval[i]:timeinterval[i+timelag], t_pred)
  
  #Return
  predictions <- model.dcw$result.prediction
  predictions <- predictions[-1]
  predictions.return.dcw <- data.frame()
  for(t in 1:nrow(predictions)){
    predictions.return.dcw[t,1] <- get.portfolio.variance(as.matrix(predictions[t,]), as.matrix(list.realCov.Date$R[,,timeinterval[i+timelag] + t]))
    predictions.return.dcw[t,2] <- get.portfolio.mean.return(as.matrix(predictions[t,]), as.matrix(return[timeinterval[i+timelag] + t,]))
  }
  results.DCW <- rbind(results.DCW,apply(predictions.return.dcw, 2, mean))
}
colnames(results.DCW) <- c("Port.Variance.DCW", "Port.Return.DCW")
