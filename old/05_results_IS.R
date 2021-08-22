
get.HAR.model.IS <- function(interval, choice.restriction, pred.days, list.realCov.Date){
  #install.packages("HARModel")
  predictions = NULL 
  predictions.var = NULL
  predictions.var_i = NULL 
  for(i in 1:30){
    data.train <- get.data.HAR(interval, i ,list.realCov.Date)
    lm.VT_i = lm(s_i~volatd+volatw+volatm, data = data.train)
    predictions.var_i = data.train$s_i - lm.VT_i$residuals
    predictions.var = cbind(predictions.var_i, predictions.var)
  }
  predictions.var <- as.data.frame(predictions.var)
  colnames(predictions.var) <- lable
  #get Weights
  predictions = NULL
  cov.HAR = NULL
  for(i in 1:nrow(predictions.var)){
    cov.HAR <- diag(predictions.var[i,])
    
    if(choice.restriction == "yes"){
      weigths <- get.realized.optimal.portfolio.weights.positive(cov.HAR)
      weigths <- t(weigths)
    }else if(choice.restriction == "no"){
      weigths <- get.realized.optimal.portfolio.weights(cov.HAR)
    }
    
    predictions <- cbind(predictions, weigths)
  }
  predictions <- t(predictions)
  colnames(predictions) <- lable
  predictions <- as.data.frame(predictions)
  return(list(weights = predictions))
}

model.equal <- get.Equal.model(return, "no", 1)
model.equal$weights <- do.call("rbind", replicate(3272, model.equal$weights, simplify = FALSE))

model.static <- get.Static.model(return, "no", lable, timeinterval[1]:timeinterval[14], 1)
model.static$weights <- do.call("rbind", replicate(3272, model.static$weights, simplify = FALSE))

model.dcw <- get.DCW.model(realoptweights, timeinterval[1]:timeinterval[14], 1)
model.dcw$result.omega <- model.dcw$result.omega[-1]
model.rw <- get.RW.model(realoptweights, timeinterval[1]:timeinterval[14], 1)
model.har <- get.HAR.model.IS(timeinterval[1]:timeinterval[14], "no", 5, list.realCov.Date)
extend <- model.har$weights[1:23,]
model.har$weights <- rbind(extend, model.har$weights)


df.pred <- cbind(df.date ,realoptweights$MSFT, model.equal$weights$MSFT, model.static$weights$MSFT, model.dcw$result.omega$MSFT,  model.rw$weights$MSFT, model.har$weights$MSFT)
colnames(df.pred) <- c("date", "Realizedoptimalportfolioweights", "Naive", "Static", "DCW", "RW", "VT")

plot.pred <- df.pred %>%
  ggplot()+
  geom_line(aes(x = date, y = RW, color = "RW"))+
  geom_line(aes(x = date, y = Realizedoptimalportfolioweights, color = "Realizedoptimalportfolioweights"))+
  geom_line(aes(x = date, y = DCW, color = "DCW"))+
  geom_line(aes(x = date, y = VT, color = "VT"))+
  geom_line(aes(x = date, y = Static, color = "Static"))+
  geom_line(aes(x = date, y = Naive, color = "Naive"))+
  scale_colour_manual("", 
                      breaks = c("RW", "Realizedoptimalportfolioweights", "DCW", "VT", "Static", "Naive"),
                      values = c("#029996", "darkgrey", "#c41818", "#646948", "#23e3fc", "#020036")) +
  labs(x ="Date", y = "Weights")+
  theme_economist()

jpeg("./Plot/Plot4_Weights_Prediction_MSFT_ALL.jpeg", width = 1000, height = 500)
plot.pred
dev.off()


portfolio.var.return.naive <- data.frame()
portfolio.var.return.static <- data.frame()
portfolio.var.return.dcw <- data.frame()
portfolio.var.return.RW <- data.frame()
portfolio.var.return.VT <- data.frame()

#REturn and Variance
for(t in 1:nrow(return)){
  portfolio.var.return.naive[t,1] <- get.portfolio.variance(as.matrix(model.equal$weights[t,]), as.matrix(list.realCov.Date$R[,,t]))
  portfolio.var.return.naive[t,2] <- get.portfolio.mean.return(as.matrix(model.equal$weights[t,]), as.matrix(return[t,]))
  portfolio.var.return.static[t,1] <- get.portfolio.variance(as.matrix(model.static$weights[t,]), as.matrix(list.realCov.Date$R[,,t]))
  portfolio.var.return.static[t,2] <- get.portfolio.mean.return(as.matrix(model.static$weights[t,]), as.matrix(return[t,]))
  portfolio.var.return.dcw[t,1] <- get.portfolio.variance(as.matrix(model.dcw$result.omega[t,]), as.matrix(list.realCov.Date$R[,,t]))
  portfolio.var.return.dcw[t,2] <- get.portfolio.mean.return(as.matrix(model.dcw$result.omega[t,]), as.matrix(return[t,]))
  portfolio.var.return.RW[t,1] <- get.portfolio.variance(as.matrix(model.rw$weights[t,]), as.matrix(list.realCov.Date$R[,,t]))
  portfolio.var.return.RW[t,2] <- get.portfolio.mean.return(as.matrix(model.rw$weights[t,]), as.matrix(return[t,]))
  portfolio.var.return.VT[t,1] <- get.portfolio.variance(as.matrix(model.har$weights[t,]), as.matrix(list.realCov.Date$R[,,t]))
  portfolio.var.return.VT[t,2] <- get.portfolio.mean.return(as.matrix(model.har$weights[t,]), as.matrix(return[t,]))
}

colnames(portfolio.var.return.naive) <- c("Port.Variance", "Port.Return")
portfolio.var.return.naive$Model <- "Naive"
colnames(portfolio.var.return.static) <- c("Port.Variance", "Port.Return")
portfolio.var.return.static$Model <- "Static"
colnames(portfolio.var.return.dcw) <- c("Port.Variance", "Port.Return")
portfolio.var.return.dcw$Model <- "DCW"
colnames(portfolio.var.return.RW) <- c("Port.Variance", "Port.Return")
portfolio.var.return.RW$Model <- "RW"
colnames(portfolio.var.return.VT) <- c("Port.Variance", "Port.Return")
portfolio.var.return.VT$Model <- "VT"
portfolio.var.return <- rbind(cbind(df.date,portfolio.var.return.naive), cbind(df.date,portfolio.var.return.static),cbind(df.date,portfolio.var.return.dcw), cbind(df.date,portfolio.var.return.RW), cbind(df.date,portfolio.var.return.VT))
portfolio.var.return <- as.data.frame(portfolio.var.return)


portfolio.var.return.summary <- portfolio.var.return %>% 
  group_by(Model) %>% 
  summarise(mean.Variance = mean(Port.Variance),
            median.Variance = median(Port.Variance),
            min.Variance = min(Port.Variance),
            max.Variance = max(Port.Variance),
            mean.Return = mean(Port.Return),
            min.Return = min(Port.Return),
            max.Return = max(Port.Return))
