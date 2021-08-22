#source
{
  source("./function.R")
}
#library
{
  library(R.matlab)
  library(tidyverse)
  library(moments)
  #install.packages("rmgarch") 
  #install.packages("PerformanceAnalytics") 
  #library(rmgarch) 
  #library(PerformanceAnalytics)
  #install.packages("xts")
  #library(xts)
  #for Plots
  #install.packages("ggthemes") # Install 
  library(ggthemes) # Load
}
#Variables
{
  file.realcov <- "Cluster_30D_realcov_cleaner_liquiditysorting.mat"
  file.return <- "return_computer.mat"
}
#Import Data
{
  lable_sector <- readxl::read_xlsx("Labels.xlsx")
  lable <- as.matrix(t(lable_sector[1]))
  
  list.realCov.Date <- readMat(file.realcov)
  return <- readMat(file.return)
  return <- return$r
  
  df.date <- as.data.frame(t(list.realCov.Date$DATE.R))
  colnames(df.date) <- "date"
  df.date$date <- as.Date(as.character(df.date$date), "%Y%m%d")
  
  df.return <- cbind(df.date, as.data.frame(return))
  colnames(df.return) <- c("date", lable[1,])
  df.return <- df.return %>% gather("stock", "return", -date)
}
#Descriptive Statistic for Returns
{
  df.descriptive.return <- df.return %>% 
    group_by(stock) %>% 
    summarise(days = n(),
              mean = round(mean(return), 4),
              sd = round(sd(return), 4),
              skewness = round(skewness(return), 4), 
              kurtosis = round(kurtosis(return), 4),
              min = round(min(return), 4),
              max = round(max(return), 4))
  
  df.descriptive.return.all <- df.return %>% 
    summarise(days = n(),
              mean = round(mean(return), 4),
              sd = round(sd(return), 4),
              skewness = round(skewness(return), 4), 
              kurtosis = round(kurtosis(return), 4),
              min = round(min(return), 4),
              max = round(max(return), 4))
  
  #Descriptive graph for the monthly return series are given in the plot.return graph.
  #The plot suggests that the return is stationary. 
  plot.returns.MSFT <- df.return %>% 
    filter(stock %in% c("MSFT")) %>% 
    ggplot(aes(x = date, y = return))+
    geom_line()+
    labs(         x ="Date", y = "Return")#title="Microsoft - Return",

  #Plot ACF and PCF 
  #The two plots show the ACF and PACF estimates given as the correlogram plot resprectively. 
  acf(return[,2])
  #The pattern of ACF shows that there is a strong correlation at lag 1 and 9. 
  pacf(return[,1])
  #The results from the ACF and PACF function suggest that the ??? return series can be modeled by some ARMA process. 
  
  #Realised Volatility 
  df.variance.stock <- as.data.frame(list.realCov.Date$R[2,2,])
  df.variance.stock <- cbind(df.date, df.variance.stock)
  colnames(df.variance.stock) <- c("date", "variance")
  plot.variance.stock <- df.variance.stock %>% 
    ggplot(aes(date, variance))+
    geom_line()
  
  #Cov Plot
  df.cov.stock <- as.data.frame(list.realCov.Date$R[1,5,])
  df.cov.stock <- cbind(df.date, df.cov.stock)
  colnames(df.cov.stock) <- c("date", "covariance")
  plot.cov.stock <- df.cov.stock %>% 
    ggplot(aes(date, covariance))+
    geom_line()
}
#Static - Unconditional Model - optimal weigths 
{
  cov.uncond <- cov(return)
  optimalweights.uncond <- get.realized.optimal.portfolio.weights(cov.uncond)
  optimalweights.uncond <- as.data.frame(t(optimalweights.uncond))
  colnames(optimalweights.uncond) <- c(lable[1,])
}
#Calculate realized optimal portfolio weights
{
  #realized optimal portfolio weights
  realoptweights <- data.frame()
  for(i in 1:3272){
    cov <- list.realCov.Date$R[,,i]
    realoptweights_i <- get.realized.optimal.portfolio.weights(cov)
    realoptweights <- rbind(realoptweights, t(realoptweights_i))
  }
  realoptweights <- cbind(df.date, realoptweights)
  colnames(realoptweights) <- c("date", lable[1,])
}
#Calculate realized optimal portfolio weights: No-short-Portfolio
{
  sum(0 < realoptweights) /(nrow(realoptweights) * ncol(realoptweights))
  sum(0 > realoptweights) /(nrow(realoptweights) * ncol(realoptweights))
  
  #Use solve.QP() to solte quadratic problem
  #install.packages("quadprog")
  library(quadprog)
  d_vector <- rep(0, 30)
  A_matrix <- cbind(rep(1, 30), diag(30))
  b_vector <- c(1, rep(0, 30))
  
  realoptweights.positive <- data.frame()
  
  for(t in 1:nrow(realoptweights)){
    D_matrix <- 2 * list.realCov.Date$R[,,t]
    
    weights <- solve.QP(Dmat = D_matrix, dvec = d_vector, Amat = A_matrix, bvec = b_vector, meq = 1)
    realoptweights.positive <- rbind(realoptweights.positive, weights$solution)
  }
  colnames(realoptweights.positive) <- lable
  realoptweights.positive <- round(realoptweights.positive, 10)
  realoptweights.positive <- cbind(df.date, realoptweights.positive)
  df.realoptweights.positive <- realoptweights.positive %>% gather("stock", "realoptweights", - date)
}
#Descriptive Statistic for realized optimal portfolio weights
{
  df.realoptweights <- realoptweights %>% gather("stock", "realoptweights", - date)
  
  df.realoptweights <- left_join(df.realoptweights, lable_sector, by = "stock")
  
  df.realoptweights.sector <- df.realoptweights %>% 
    group_by(sector, date) %>% 
    summarise(realoptweights = sum(realoptweights))%>%
    group_by(date) %>% 
    mutate(realoptweights_scaled = abs(realoptweights / sum(abs(realoptweights))))
  
  df.realoptweights %>% filter(stock == "MSFT") %>% ggplot(aes(x = date, y = realoptweights))+
    geom_line()+
    labs(x ="Date", y = "Realized Optimal Portfolio Weights")+ 
    theme_economist()
  
    
  
  df.descriptive.weights <- df.realoptweights %>% 
    group_by(`stock`) %>% 
    summarise(days = n(),
              mean = round(mean(realoptweights), 4),
              sd = round(sd(realoptweights), 4),
              skewness = round(skewness(realoptweights), 4), 
              kurtosis = round(kurtosis(realoptweights), 4),
              min = round(min(realoptweights), 4),
              max = round(max(realoptweights), 4))
  
  df.descriptive.weights.all <- df.realoptweights %>% 
    summarise(days = n(),
              mean = round(mean(realoptweights), 4),
              sd = round(sd(realoptweights), 4),
              skewness = round(skewness(realoptweights), 4), 
              kurtosis = round(kurtosis(realoptweights), 4),
              min = round(min(realoptweights), 4),
              max = round(max(realoptweights), 4))
  
  #Descriptive graph for the monthly return series are given in the plot.return graph.
  #The plot suggests that the return is stationary. 
  plot.realoptimalweigths <- df.realoptweights %>% 
    filter(stock %in% c("MSFT")) %>% 
    ggplot(aes(x = date, y = realoptweights))+
    geom_line()+
    labs(title="Realized Optimal Portfolio Weights from 31. December 2001 to 31. December 2014",
         x ="Date", y = "Optimal Weights")
  
  #Plot ACF and PCF 
  #The two plots show the ACF and PACF estimates given as the correlogram plot resprectively. 
  acf.all <- data.frame()
  for(i in 2:ncol(realoptweights)){
    acf_i <- acf(realoptweights[,i])
    
    acf_i <- cbind(acf_i$lag, acf_i$acf)
    acf_i <- as.data.frame(acf_i)
    colnames(acf_i) <- c("Lag", "ACF")
    acf_i$Stock <- lable[i-1]
    acf.all <- rbind(acf.all, acf_i)
  }
  
  acf.all %>% 
    filter(Lag == 1) %>%
    ggplot(aes(reorder(Stock, -ACF), ACF))+
    geom_col()


  #The pattern of ACF shows that there is a strong correlation at all lags. 
  pacf(realoptweights[,2])
  #The results from the ACF and PACF function suggest that the ??? return series can be modeled by some ARMA process. 
}
#Standard ARMA model - DCW
{
  #Unrestricted
  #estimate coefficients for all time series
  dcw.results.full <- get.DCW.model(realoptweights, 1:3272)

  coefficients$B <- -dcw.results.full$result.coefficients$ma1
  coefficients$A <- dcw.results.full$result.coefficients$ar1 + dcw.results.full$result.coefficients$ma1
  coefficients$AplusB <- dcw.results.full$result.coefficients$ar1
  
  summary_coeff <- data.frame(row.names = c("mean", "min", "max", "quant5%", "quant95%", "sd"))
  summary_coeff$A <- c(mean(coefficients$A),
                       min(coefficients$A),
                       max(coefficients$A), 
                       quantile(coefficients$A, 0.05),
                       quantile(coefficients$A, 0.95),
                       sd(coefficients$A))
  summary_coeff$B <- c(mean(coefficients$B),
                        min(coefficients$B),
                        max(coefficients$B), 
                       quantile(coefficients$B, 0.05),
                       quantile(coefficients$B, 0.95),
                       sd(coefficients$B))
  
  summary_coeff$AplusB <- c(mean(coefficients$ar1),
                            min(coefficients$ar1),
                            max(coefficients$ar1), 
                            quantile(coefficients$ar1, 0.05),
                            quantile(coefficients$ar1, 0.95),
                            sd(coefficients$ar1))

  df.pred.tidy <- df.pred %>% gather("Model", "Weight", -date)
  #Restricted
  dcw.rest.results.full <- get.DCW.model(realoptweights.positive, 1:3272)
  
  df.pred.positiv <- cbind(df.date ,realoptweights.positive$MSFT, dcw.rest.results.full$result.omega$MSFT)
  colnames(df.pred.positiv) <- c("date", "Realized optimal portfolio weights", "DCW")
  df.pred.positiv <- df.pred.positiv %>% transform( `Realized optimal portfolio weights` = as.numeric(`Realized optimal portfolio weights`),
                                    DCW = as.numeric(DCW))
} 
#DCC
{
  library(rugarch)
  library(rmgarch)
  garchspec = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                         variance.model = list(garchOrder = c(1,1),
                                               model = "sGARCH"), distribution.model = "norm") 
  dcc.garchsnpdji.spec = dccspec(uspec = multispec( replicate(30, garchspec) ), dccOrder = c(1,1), distribution = "mvnorm")
  dcc_fit = dccfit(dcc.garchsnpdji.spec , data = return[,1:30], fit.control=list(scale=TRUE)) 
  dcc_fit
  dccforecast(dcc_fit,  n.ahead = 1)
  cor1 <- rcor(dcc_fit)
  DCC_realoptweights <- data.frame()
  for(i in 1:3272){
    cov <- cor1[,,i]
    unity.vector <- t(vec.unity(ncol(cov)))
    DCC_realoptweights_i <- solve(unity.vector %*% solve(cov) %*% t(unity.vector))[[1]] * solve(cov) %*% t(unity.vector)
    DCC_realoptweights <- rbind(DCC_realoptweights, t(DCC_realoptweights_i))
  }
  ts.plot(DCC_realoptweights[5])
  ts.plot(realoptweights[2])
  DCC_epsilon <- DCC_realoptweights - realoptweights[-1]
  ts.plot(DCC_epsilon)
  #Plot Prediction
  ts.plot(realoptweights[31])
  points(DCC_realoptweights[30], type = "l", col = 4, lty = 1)
  points(omega[31], type = "l", col = 2, lty = 1)
}
#Random walk 
{
  ## initialize {x_t} and {w_t}
  optweights.RW <- rbind(realoptweights[1,], realoptweights)
  optweights.RW <- optweights.RW[-nrow(optweights.RW),]
  optweights.RW <- optweights.RW[-1]
  optweights.RW <- cbind(df.date, optweights.RW)
  colnames(optweights.RW) <- c("date", lable[1,])
}
#Turnover
{
  get_TO <- function(weights){
    return(1/nrow(weights) * sum(rowSums(abs(weights))))
  }
  get_PV <- function(weights){
    sum = 0
    for(i in 1:nrow(weights)){
      i=1
      cov <- list.realCov.Date$R[,,i]
      sum_t <- rowSums(weights[i,] * cov * t(weights[i,]))
      sum = sum + sum_t
    }
    PV = 1/nrow(omega) * sum 
    return(PV)
  }
  DCW_PV = get_PV(DCW_weights)
  DCC_PV = get_PV(DCC_realoptweights)
  #Turnover
  DCw_TO <- get_TO(DCW_weights)
  DCC_TO <- get_TO(DCC_realoptweights)
}
#Variance vs. Return
{
  #Variance
  get.portfolio.variance <- function(w, cv){
    port.variance = w %*% cv %*% t(w)
    return(port.variance[1,1])
  }
  #Return
  get.portfolio.mean.return <- function(w, return){
    port.return = w %*% return
    return(port.return[1])
  }
  
  portfolio.var.return.naive <- data.frame()
  portfolio.var.return.dcw <- data.frame()
  portfoli.weights.DCw <- omega[-1]
  
  portfolio.var.return.RW <- data.frame()
  portfolio.var.return.RW <- data.frame()
  portfoli.weights.RW <- optweights.RW[-1]
  
  
  #unconditional 
  for(t in 1:nrow(return)){
    portfolio.var.return.naive[t,1] <- get.portfolio.variance(as.matrix(optimalweights.uncond), as.matrix(list.realCov.Date$R[,,t]))
    portfolio.var.return.naive[t,2] <- get.portfolio.mean.return(as.matrix(optimalweights.uncond), as.matrix(return[t,]))
    portfolio.var.return.dcw[t,1] <- get.portfolio.variance(as.matrix(portfoli.weights.DCw[t,]), as.matrix(list.realCov.Date$R[,,t]))
    portfolio.var.return.dcw[t,2] <- get.portfolio.mean.return(as.matrix(portfoli.weights.DCw[t,]), as.matrix(return[t,]))
    portfolio.var.return.RW[t,1] <- get.portfolio.variance(as.matrix(portfoli.weights.RW[t,]), as.matrix(list.realCov.Date$R[,,t]))
    portfolio.var.return.RW[t,2] <- get.portfolio.mean.return(as.matrix(portfoli.weights.RW[t,]), as.matrix(return[t,]))
  }
  colnames(portfolio.var.return.naive) <- c("Port.Variance", "Port.Return")
  portfolio.var.return.naive$Model <- "Naive"
  colnames(portfolio.var.return.dcw) <- c("Port.Variance", "Port.Return")
  portfolio.var.return.dcw$Model <- "DCW"
  colnames(portfolio.var.return.RW) <- c("Port.Variance", "Port.Return")
  portfolio.var.return.RW$Model <- "RW"
  portfolio.var.return <- rbind(cbind(df.date,portfolio.var.return.naive),cbind(df.date,portfolio.var.return.dcw), cbind(df.date,portfolio.var.return.RW))
  portfolio.var.return <- as.data.frame(portfolio.var.return)
  
  portfolio.var.return.diff <- cbind(portfolio.var.return.naive,portfolio.var.return.dcw)
  colnames(portfolio.var.return.diff) <- c("Naive.Port.Variance", "Naive.Port.Return", "Model1", "DCW.Port.Variance", "DCW.Port.Return", "Model2")
  portfolio.var.return.diff$DiffReturn <- portfolio.var.return.diff$DCW.Port.Return - portfolio.var.return.diff$Naive.Port.Return
  portfolio.var.return.diff$DiffVariance <- portfolio.var.return.diff$DCW.Port.Variance - portfolio.var.return.diff$Naive.Port.Variance
  mean(portfolio.var.return.diff$DiffReturn)
  mean(portfolio.var.return.diff$DiffVariance)
  
  portfolio.var.return.summary <- portfolio.var.return %>% 
    group_by(Model) %>% 
    summarise(mean.Variance = mean(Port.Variance),
              mean.Return = mean(Port.Return))
}
#Simulation
{
  portfolio.var.random <- data.frame()
  portfolio.return.random <- data.frame()
  
  portfolio.weight.random <- matrix()
  portfolio.weight.random <- matrix(runif(3000, -1, 1), ncol=30)# matrix(runif(9000), ncol=30)
  portfolio.weight.random <- portfolio.weight.random/rowSums(portfolio.weight.random)
  
  for(t in 1:nrow(return)){
    for(i in 1:nrow(portfolio.weight.random)){
      portfolio.var.random[t,i] <- get.portfolio.variance(t(portfolio.weight.random[i,]), as.matrix(list.realCov.Date$R[,,t]))
      portfolio.return.random[t,i] <- get.portfolio.mean.return(t(portfolio.weight.random[i,]), as.matrix(return[t,]))
    }
  }
  portfolio.var.random.mean <- colMeans(portfolio.var.random)
  portfolio.return.random.mean <- colMeans(portfolio.return.random)
  portfolio.var.return.random <- rbind(portfolio.return.random.mean, portfolio.var.random.mean)
  portfolio.var.return.random <- t(portfolio.var.return.random)
  colnames(portfolio.var.return.random) <- c("Port.Return", "Port.Variance")
  
  portfolio.var.return.naive.DCW <- portfolio.var.return %>% 
    group_by(Model)%>%
    summarise(Port.Variance = mean(Port.Variance),
              Port.Return = mean(Port.Return))
}
#OOS Prediction
{
  timeinterval <- c(1,253,555,757, 1009, 1260, 1511, 1764, 2016, 2268, 2520, 2770, 3021, 3272)
  dcw.model1 <- get.DCW.model(realoptweights, timeinterval[1]:timeinterval[2],length(timeinterval[1]:timeinterval[2]))
  predictions <- dcw.model1$result.prediction
  
  df.pred.MSFT <- cbind(df.date[101:200,], predictions$MSFT, realoptweights[101:200,]$MSFT)
  df.pred.MSFT <- as.data.frame(df.pred.MSFT)
  colnames(df.pred.MSFT) <- c("date", "OOS", "realoptimalweights")
  df.pred.MSFT %>% ggplot()+
    geom_point(aes(x = date, y = OOS))+
    geom_point(aes(x = date, y = realoptimalweights))
  
  #Return
  predictions.return.dcw <- data.frame()
  predictions <- predictions[-1]
  for(t in 1:nrow(predictions)){
    predictions.return.dcw[t,1] <- get.portfolio.variance(as.matrix(predictions[t,]), as.matrix(list.realCov.Date$R[,,t+100]))
    predictions.return.dcw[t,2] <- get.portfolio.mean.return(as.matrix(predictions[t,]), as.matrix(return[t+100,]))
  }
  colnames(predictions.return.dcw) <- c("Port.Variance", "Port.Return")
  apply(predictions.return.dcw, 2, mean)
}

