rm(list = ls())
#---Load all packages and functions---
{
  #---functions---
  file.sources = list.files(path = "./code/", pattern="*.R")
  sapply(paste0("./code/",file.sources),source,.GlobalEnv)
  #packages
  names.packages <- c("R.matlab", #Read Matlab file
                "quadprog",#quadratic optimization for restiction 
                "ggthemes", #for ggplot
                "ggplot2", #to plot graphs
                "tidyverse"
  )
  get.packages(names.packages)
}
#---Import Data---
{
  #Variables
  file.realcov <- "./data/Cluster_30D_realcov_cleaner_liquiditysorting.mat"
  file.return <- "./data/return_computer.mat"
  file.lable <- "./data/Labels.xlsx"
  data <- get.Import.Data(file.realcov, file.return, file.lable)
  T = as.numeric(nrow(data$date)[1])
}
#---Calculate realized optimal portfolio weights---
{
  #without restriction
  realoptweights <- data.frame()
  for(i in 1:T){
    cov <- data$Cov[,,i]
    realoptweights_i <- get.realized.optimal.portfolio.weights(cov)
    realoptweights <- rbind(realoptweights, realoptweights_i)
  }
  realoptweights <- as.matrix(realoptweights)
  
  #With restriction
  print(paste0("Percentage of short positions: ", 100 *  sum(0 < realoptweights) /(nrow(realoptweights) * ncol(realoptweights))))
  print(paste0("Percentage of long positions: ", 100 *  sum(0 > realoptweights) /(nrow(realoptweights) * ncol(realoptweights))))
  
  n <- ncol(data$R)
  d_vector <- rep(0, n)
  A_matrix <- cbind(rep(1, n), diag(n))
  b_vector <- c(1, rep(0, n))
  
  realoptweights.positive <- data.frame()
  
  for(t in 1:T){
    D_matrix <- 2 * data$Cov[,,t]
    
    weights <- solve.QP(Dmat = D_matrix, dvec = d_vector, Amat = A_matrix, bvec = b_vector, meq = 1)
    realoptweights.positive <- rbind(realoptweights.positive, weights$solution)
  }
  realoptweights.positive <- as.matrix(realoptweights.positive)
  omega <- list(real.omega.norest = realoptweights, 
                real.omega.norest.desc = get.Desc.Stat(realoptweights),
                real.omega.rest = realoptweights.positive, 
                real.omega.rest.desc = get.Desc.Stat(realoptweights.positive))
  
  
  rm(weights, cov, realoptweights_i, realoptweights, realoptweights.positive, n, D_matrix, d_vector, A_matrix, b_vector)
}
#In-Sample
{
  start = 1
  end = T
  #Get optimal estimated weights for each model
  omega$omega.Equal <- get.Equal.model(data$R, IS.start =  start, IS.end =  end) #weigths from 1 to T 
  omega$omega.Static <- get.Static.model(data$R, IS.start =  start, IS.end =  end) #weigths from 1 to T 
  omega$omega.RW <- get.RW.model(omega$real.omega.norest, omega$real.omega.rest, IS.start =  start, IS.end =  end)
  omega$omega.DCW <- get.DCW.model(omega$real.omega.norest, omega$real.omega.rest, IS.start =  start, IS.end =  end)
  omega$omega.VAR <- get.HAR.model(data$Cov, IS.start =  start, IS.end =  end)
  
  #Performance measures
  measures <- list()
  measures$IS <- NULL
  measures = cbind(
  get.portfolio.mean.return(omega$omega.Equal$omega.IS.norest, data$R),
  get.portfolio.mean.return(omega$omega.Static$omega.IS.norest, data$R),
  get.portfolio.mean.return(omega$omega.RW$omega.IS.norest, data$R),
  get.portfolio.mean.return(omega$omega.DCW$omega.IS.norest, data$R),
  get.portfolio.mean.return(omega$omega.VAR$omega.IS.norest, data$R[-c(1:23),])
  )
  
  measures = rbind(measures,cbind(
  get.portfolio.variance(omega$omega.Equal$omega.IS.norest, data$Cov),
  get.portfolio.variance(omega$omega.Static$omega.IS.norest, data$Cov),
  get.portfolio.variance(omega$omega.RW$omega.IS.norest, data$Cov),
  get.portfolio.variance(omega$omega.DCW$omega.IS.norest, data$Cov),
  get.portfolio.variance(omega$omega.VAR$omega.IS.norest, data$Cov[,,-c(1:23)])
  ))
  
  measures = rbind(measures,cbind(
  get.portfolio.mean.SR(omega$omega.Equal$omega.IS.norest, data$R, data$Cov),
  get.portfolio.mean.SR(omega$omega.Static$omega.IS.norest, data$R, data$Cov),
  get.portfolio.mean.SR(omega$omega.RW$omega.IS.norest, data$R, data$Cov),
  get.portfolio.mean.SR(omega$omega.DCW$omega.IS.norest, data$R, data$Cov),
  get.portfolio.mean.SR(omega$omega.VAR$omega.IS.norest, data$R[-c(1:23),], data$Cov[,,-c(1:23)])
  ))
  colnames(measures) <- c("Equal", "Static", "RW", "DCW", "VAR")
  rownames(measures) <- c("return", "variance", "sr")
}
#OOS-Sample
{
  measures <- list()
  measures$IS <- NULL
  measures$OOS <- NULL
  measures$SR <- NULL
  measures$steps <- NULL
  measures$steps$IS.start <- NULL
  measures$steps$IS.end <- NULL
  measures$steps$OOS.start <- NULL
  measures$steps$OOS.end <- NULL
  timeinterval <- c(2,253,505,757, 1009, 
                    1260, 1511, 1764, 2016, 
                    2268, 2520, 2770) #,3021 Index for first date of each year
  data$date[timeinterval,]
  period.OOS = 30
  period.IS = 252
  for(date.intex in timeinterval){
    start.IS = date.intex 
    end.IS = date.intex + period.IS -1
    start.OOS = end.IS + 1
    end.OOS = start.OOS + period.OOS -1
    
    measures$steps$IS.start = rbind(measures$steps$IS.start, as.character(data$date[start.IS,]))
    measures$steps$IS.end = rbind(measures$steps$IS.end, as.character(data$date[end.IS,]))
    measures$steps$OOS.start = rbind(measures$steps$OOS.start, as.character(data$date[start.OOS,]))
    measures$steps$OOS.end = rbind(measures$steps$OOS.end, as.character(data$date[end.OOS,]))
    
    #Get optimal estimated weights for each model
    omega$omega.Equal <- get.Equal.model(data$R, IS.start =  start.IS, IS.end =  end.IS, OOS.start = start.OOS, OOS.end = end.OOS) #weigths from 1 to T 
    omega$omega.Static <- get.Static.model(data$R, IS.start =  start.IS, IS.end =  end.IS, OOS.start = start.OOS, OOS.end = end.OOS) #weigths from 1 to T 
    omega$omega.RW <- get.RW.model(omega$real.omega.norest, omega$real.omega.rest, IS.start =  start.IS, IS.end =  end.IS, OOS.start = start.OOS, OOS.end = end.OOS)
    omega$omega.DCW <- get.DCW.model(omega$real.omega.norest, omega$real.omega.rest, IS.start =  start.IS, IS.end =  end.IS, OOS.start = start.OOS, OOS.end = end.OOS)
    omega$omega.VAR <- get.HAR.model(data$Cov, IS.start =  start.IS, IS.end =  end.IS, OOS.start = start.OOS, OOS.end = end.OOS)
    
    #OOS - Performance measures
    measures$OOS$return = rbind( measures$OOS$return, cbind(
      get.portfolio.mean.return(omega$omega.Equal$omega.OOS.norest, data$R[c(start.OOS:end.OOS),]),
      get.portfolio.mean.return(omega$omega.Static$omega.OOS.norest, data$R[c(start.OOS:end.OOS),]),
      get.portfolio.mean.return(omega$omega.RW$omega.OOS.norest, data$R[c(start.OOS:end.OOS),]),
      get.portfolio.mean.return(omega$omega.DCW$omega.OOS.norest, data$R[c(start.OOS:end.OOS),]),
      get.portfolio.mean.return(omega$omega.VAR$omega.OOS.norest, data$R[c((start.OOS+23):end.OOS),])
    ))
    colnames(measures$OOS$return) <- c("R.Equal", "R.Static", "R.RW", "R.DCW", "R.VAR")
    
    measures$OOS$variance = rbind(measures$OOS$variance,cbind(
      get.portfolio.variance(omega$omega.Equal$omega.OOS.norest, data$Cov[,,c(start.OOS:end.OOS)]),
      get.portfolio.variance(omega$omega.Static$omega.OOS.norest, data$Cov[,,c(start.OOS:end.OOS)]),
      get.portfolio.variance(omega$omega.RW$omega.OOS.norest, data$Cov[,,c(start.OOS:end.OOS)]),
      get.portfolio.variance(omega$omega.DCW$omega.OOS.norest, data$Cov[,,c(start.OOS:end.OOS)]),
      get.portfolio.variance(omega$omega.VAR$omega.OOS.norest, data$Cov[,,c((start.OOS+23):end.OOS)])
    ))
    colnames(measures$OOS$variance) <- c("PV.Equal", "PV.Static", "PV.RW", "PV.DCW", "PV.VAR")
    
    measures$OOS$SR = rbind(measures$OOS$SR,cbind(
      get.portfolio.mean.SR(omega$omega.Equal$omega.OOS.norest, data$R[c(start.OOS:end.OOS),], data$Cov[,,c(start.OOS:end.OOS)]),
      get.portfolio.mean.SR(omega$omega.Static$omega.OOS.norest, data$R[c(start.OOS:end.OOS),], data$Cov[,,c(start.OOS:end.OOS)]),
      get.portfolio.mean.SR(omega$omega.RW$omega.OOS.norest, data$R[c(start.OOS:end.OOS),], data$Cov[,,c(start.OOS:end.OOS)]),
      get.portfolio.mean.SR(omega$omega.DCW$omega.OOS.norest, data$R[c(start.OOS:end.OOS),], data$Cov[,,c(start.OOS:end.OOS)]),
      get.portfolio.mean.SR(omega$omega.VAR$omega.OOS.norest, data$R[c((start.OOS+23):end.OOS),], data$Cov[,,c((start.OOS+23):end.OOS)])
    ))
    colnames(measures$OOS$SR) <- c("SR.Equal", "SR.Static", "SR.RW", "SR.DCW", "SR.VAR")
  }
  OOS.results <- data.frame( IS.start = measures$steps$IS.start, 
                             IS.end =  measures$steps$IS.end,
                             OOS.start = measures$steps$OOS.start, 
                             OOS.end =  measures$steps$OOS.end)
  OOS.results <- cbind(OOS.results,measures$OOS$return, measures$OOS$variance, measures$OOS$SR )
  OOS.results.summary <- colMeans(OOS.results[-c(1:4)])
}
#Plot data
{
  get.plot.return(data$R, data$date, data$lable, "MSFT")
  get.plot.omega(omega$real.omega.rest, data$date, data$lable, "MSFT")
}


