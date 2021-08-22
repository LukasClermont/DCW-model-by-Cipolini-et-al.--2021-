timeinterval <- c(1,253,505,757, 1009, 1260, 1511, 1764, 2016, 2268, 2520, 2770, 3021, 3272)
for(period in 1:3){
  period = 2
  restriction = "yes" #no / yes
  t_pred <- c(30, 90, 180)
  t_pred <- t_pred[period]
  timelag <- c(1,2,3)
  timelag <- timelag[period]
  intervals <- length(timeinterval)-timelag - 1
  days.timeinterval <- df.date[timeinterval,]
  df.interval.IS <- NULL 
  df.interval.OOS <- NULL 
  for(i in 1:intervals){
    df.interval.IS <- rbind(df.interval.IS, paste0("IS - Lag ", timelag ," - ",df.date[timeinterval[i],], " - ",df.date[timeinterval[i+timelag],]))
    df.interval.OOS <- rbind(df.interval.OOS, paste0("OS - Lag ", timelag ," - ",df.date[timeinterval[i+timelag],], " - ",df.date[timeinterval[i+timelag],]+days(t_pred)))
  }
  results.equal <- get.model.summary("Equal", choice.restriction = restriction , intervals, timelag, timeinterval, list.realCov.Date, return, t_pred)
  results.static <- get.model.summary("Static", choice.restriction = restriction , intervals, timelag, timeinterval, list.realCov.Date, return, t_pred)
  results.RW <- get.model.summary("RW", choice.restriction = restriction , intervals, timelag, timeinterval, list.realCov.Date, return, t_pred)
  results.DCW <- get.model.summary("DCW", choice.restriction = restriction , intervals, timelag, timeinterval, list.realCov.Date, return, t_pred)
  results.HAR <- get.model.summary("HAR", choice.restriction = restriction , intervals, timelag, timeinterval, list.realCov.Date, return, t_pred)
  
  #Positiv
  results.equal.positiv <- rowSums(results.equal$prediction==0)
  results.static.positiv <- rowSums(results.static$prediction==0)
  results.RW.positiv <- rowSums(results.RW$prediction==0)
  results.DCW.positiv <- rowSums(round(results.DCW$prediction, 4)<=0)
  
  results.prediction <- cbind(nrow(results.equal$prediction),nrow(results.static$prediction), nrow(results.RW$prediction), nrow(results.HAR$prediction), nrow(results.DCW$prediction))
  results <- cbind(results.equal$restults,results.static$restults, results.RW$restults, results.HAR$restults, results.DCW$restults)
  results$minVariance <- apply(results[c(1,3,5,7,9)], 1, FUN=min)
  results$maxReturn <- apply(results[c(2,4,6,8,10)], 1, FUN=max)
  results <- results %>% 
    mutate(bestModelVAR = case_when(minVariance == Port.VarianceStatic ~ "Static", 
                                 minVariance == Port.VarianceRW ~ "RW", 
                                 minVariance == Port.VarianceHAR ~ "VT", 
                                 minVariance == Port.VarianceEqual ~ "Naive", 
                                 minVariance == Port.VarianceDCW ~ "DCW"),
           bestModelRerturn = case_when(maxReturn == Port.ReturnStatic ~ "Static", 
                                    maxReturn == Port.ReturnRW ~ "RW", 
                                    maxReturn == Port.ReturnHAR ~ "VT", 
                                    maxReturn == Port.ReturnEqual ~ "Naive", 
                                    maxReturn == Port.ReturnDCW ~ "DCW"))
  
  
  print(results[c(1,3,5,7,9)])
  colMeans(print(results[c(1,3,5,7,9)]))
  print(results[c(2,4,6,8,10)])
  colMeans(print(results[c(2,4,6,8,10)]))
  colMeans(print(results[1:10]))
  write.csv2(results, paste0("./data/temp/",period,"_resultsperiod",period,"Restriction_",restriction,".csv"))
  write.csv2(colMeans(results[1:10]), paste0("./data/temp/",period,"_resultsperiodsummary",period,"Restriction_",restriction,".csv"))
  write.csv2(df.interval.IS, paste0("./data/temp/",period,"_ISPeriods",period,"Restriction_",restriction,".csv"))
  write.csv2(df.interval.OOS, paste0("./data/temp/",period,"_OOSPeriods",period,"Restriction_",restriction,".csv"))
}
#Plot density
densplot2 <- ggplot() + 
  geom_density(data = results.DCW$variance_timeline, aes(x=Port.Variance, colour = "DCW"))+
  geom_density(data = results.equal$variance_timeline, aes(x=Port.Variance, colour = "Naive"))+
  geom_density(data = results.RW$variance_timeline, aes(x=Port.Variance, colour = "RW"))+
  geom_density(data = results.static$variance_timeline, aes(x=Port.Variance, colour = "Static"))+
  geom_density(data = results.HAR$variance_timeline, aes(x=Port.Variance, colour = "VT"))+
  scale_colour_manual("", 
                      breaks = c("RW", "Realizedoptimalportfolioweights", "DCW", "VT", "Static", "Naive"),
                      values = c("#029996", "darkgrey", "#c41818", "#646948", "#23e3fc", "#020036")) +
  labs(x ="Variance", y = "Density")+
  theme_economist()
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(densplot1)

densplot <- grid.arrange(arrangeGrob(densplot1 + ylim(0,6) + theme(legend.position = "none") + ggtitle("1"),
                         densplot2+ ylim(0,6)+ theme(legend.position = "none")+ ggtitle("2"),
                         densplot3+ ylim(0,6)+ theme(legend.position = "none")+ ggtitle("3"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1))

