get.model.summary <- function(Modeltype, choice.restriction, intervals, timelag,timeinterval, list.realCov.Date, return, t_pred){
  predictions.return <- data.frame()
  predictions.var.t <- NULL
  results <- data.frame()
  prediction.model <- data.frame()
  for(i in 1:intervals){
    if(Modeltype == "Equal"){
      model.equal <- get.Equal.model(return, choice.restriction, t_pred)
      predictions <- model.equal$prediction
    } else if (Modeltype == "Static"){
      model.static <- get.Static.model(return, choice.restriction, lable, timeinterval[i]:timeinterval[i+timelag], t_pred)
      predictions <- model.static$prediction
    }else if(Modeltype == "DCW"){
      if(choice.restriction == "yes"){
        realisedportfolioweights <- realoptweights.positive
      } else if(choice.restriction == "no"){
        realisedportfolioweights <- realoptweights
      }
      model.dcw <- get.DCW.model(realisedportfolioweights, timeinterval[i]:timeinterval[i+timelag], t_pred)
      predictions <- model.dcw$result.prediction
      predictions <- predictions[-1]
    }else if(Modeltype == "RW"){
      if(choice.restriction == "yes"){
        model.rw <- get.RW.model(realoptweights.positive, timeinterval[i]:timeinterval[i+timelag], t_pred)
      } else if(choice.restriction == "no"){
        model.rw <- get.RW.model(realoptweights, timeinterval[i]:timeinterval[i+timelag], t_pred)
      }
      predictions <- model.rw$prediction
    }else if(Modeltype == "HAR"){
      model.har <- get.HAR.model(timeinterval[i]:timeinterval[i+timelag], choice.restriction, t_pred, list.realCov.Date)
      predictions <- model.har$prediction
    }
    #Return
    for(t in 1:nrow(predictions)){
      predictions.return[t,1] <- get.portfolio.variance(as.matrix(predictions[t,]), as.matrix(list.realCov.Date$R[,,timeinterval[i+timelag] + t]))
      predictions.return[t,2] <- get.portfolio.mean.return(as.matrix(predictions[t,]), as.matrix(return[timeinterval[i+timelag] + t,]))
      predictions.return[t,3] <- get.portfolio.mean.return(as.matrix(predictions[t,]), as.matrix(return[timeinterval[i+timelag] + t,]), as.matrix(list.realCov.Date$R[,,timeinterval[i+timelag] + t]))
    }
    colnames(predictions.return) <- c("Port.Variance", "Port.Return", "Port.SR")
    predictions.var.t <- predictions.return
    predictions.return <- predictions.return %>% 
      summarise(Port.Variance = mean(Port.Variance),
                Port.Return = mean(Port.Return),
                Port.SR = mean(Port.SR))
    results <- rbind(results,predictions.return)
  }
  colnames(results) <- c(paste0("Port.Variance", Modeltype), paste0("Port.Return", Modeltype), paste0("Port.SR", Modeltype))
  return(list(restults = results, prediction = predictions, variance_timeline = predictions.var.t))
}