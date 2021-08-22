get.data.HAR <- function(interval, i ,list.realCov.Date){
  volatw = NULL 
  volatm = NULL 
  volatd = NULL
  s_i <- list.realCov.Date$R[i,i,interval]
  for (t in 23:length(s_i)){
    volatd[t] = s_i[(t-1)]
    volatw[t] = sum(s_i[(t-5):(t-1)],na.rm = T)/5
    volatm[t] = sum(s_i[(t-22):(t-1)],na.rm = T)/22 
  }
  data.HAR <- as.data.frame(cbind(s_i, volatd, volatw, volatm))
  return(data.HAR[-c(1:23),])
}

get.HAR.model <- function(interval, pred.days, list.realCov.Date){
  #install.packages("HARModel")
  interval = timeinterval[i]:timeinterval[i+timelag]
  pred.days = t_pred
  predictions = NULL 
  predictions.var = NULL
  for(i.asset in 1:30){
    data.train <- get.data.HAR(interval, i.asset ,list.realCov.Date)
    lm.VT_i = lm(s_i~volatd+volatw+volatm, data = data.train)
    data.test <- get.data.HAR(max(interval-21):max(interval+pred.days), i.asset ,list.realCov.Date)
    predictions_i <- predict(lm.VT_i, newdata = data.test)
    predictions <- cbind(predictions, predictions_i)
  }
  predictions.var <- as.data.frame(predictions)
  colnames(predictions.var) <- lable
  #get Weights
  predictions = NULL
  cov.HAR = NULL
  for(i in 1:nrow(predictions.var)){
    cov.HAR <- diag(predictions.var[i,])
    weigths <- get.realized.optimal.portfolio.weights(cov.HAR)
    predictions <- cbind(predictions, weigths)
  }
  predictions <- t(predictions)
  colnames(predictions) <- lable
  predictions <- as.data.frame(predictions)
  return(list(predictions = predictions))
}


