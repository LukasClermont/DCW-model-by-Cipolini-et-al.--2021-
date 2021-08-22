results.static <- data.frame()

results.static <- get.model.summary("Static", intervals, timelag, timeinterval, list.realCov.Date, return)

get.model.summary(intervals, timelag, timeinterval, predictions, list.realCov.Date, return)

for(i in 1:intervals){
  model.static <- get.Static.model(return, lable, timeinterval[i]:timeinterval[i+timelag], t_pred)
  
  #Return
  predictions <- model.static$prediction
  predictions.return.static <- data.frame()
  for(t in 1:nrow(predictions)){
    predictions.return.static[t,1] <- get.portfolio.variance(as.matrix(predictions[t,]), as.matrix(list.realCov.Date$R[,,timeinterval[i+timelag] + t]))
    predictions.return.static[t,2] <- get.portfolio.mean.return(as.matrix(predictions[t,]), as.matrix(return[timeinterval[i+timelag] + t,]))
  }
  results.static <- rbind(results.static,apply(predictions.return.static, 2, mean))
}
colnames(results.static) <- c("Port.Variance.Static", "Port.Return.Static")
