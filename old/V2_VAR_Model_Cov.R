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
#Naive - Unconditional Model - optimal weigths 
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
#Test Statistics
{
  Box.test(y-mean(y),type='Ljung-Box')
  Box.test((y-mean(y))^2,type='Ljung-Box')
  
  #Phillips-Perron stationary test
  pptab <- NULL 
  for(i in 2:ncol(realoptweights)){
    pp <- PP.test(realoptweights[,i])
    pptab <- rbind(pptab, pp$p.value) 
  }
  #->All Variables are stationary
  
  #Lijung Box test - weights
  LBtest.weights.pvalue <- NULL
  LBtest.weights.statistic <- NULL 
  for(i in 2:ncol(realoptweights)){
    lb_i <- Box.test(realoptweights[,i], lag = 10, type = "Ljung-Box")
    LBtest.weights.pvalue <- rbind(LBtest.weights.pvalue, lb_i$p.value) 
    LBtest.weights.statistic <- rbind(LBtest.weights.statistic, lb_i$statistic) 
  }
  LBtest <- cbind(LBtest.weights.statistic, LBtest.weights.pvalue)
  LBtest <- round(LBtest, 2)
}
#Standard ARMA model
{
  y <- realoptweights$MSFT
  Model_ARMA <- c(1, 0, 1) # MA :  c(0, 0, 1) AR: c(1, 0, 0)
  msft_arma <- arima(y, order = Model_ARMA, method ="ML")
  
  AR_fit <- y - residuals(msft_ma)
  
  ts.plot(y)
  points(AR_fit, type = "l", col = 2, lty = 2)
  
  
  #estimate coefficients for all time series
  coefficients <- data.frame(ar1 = NA, ma1 = NA, ma2= NA , intercept = NA)
  coefficients <- coefficients %>% filter(is.na(ar1) == FALSE)
  Modeltype_ARMA <- c(1, 0, 1)
  epsilon <- realoptweights[1]
  omega <- realoptweights[1]
  data_aic <- data.frame()
  for(i in 2:ncol(realoptweights)){
    y <- ts(realoptweights[i])
    arma11 <- arima(y, order = Modeltype_ARMA, method ="ML")
    epsilon_t <- as.data.frame(residuals(arma11))
    colnames(epsilon_t) <- paste0("V",i-1)
    epsilon <- cbind(epsilon, epsilon_t)
    omega_t <- as.data.frame(y) - epsilon_t
    colnames(omega_t) <- i-1
    omega <- cbind(omega, omega_t)
    
    coeff_i <- as.data.frame(t(arma11$coef))
    coefficients <- rbind(coefficients, coeff_i)
  }
  colnames(omega) <- c("date", lable[1,])
  colnames(epsilon) <- c("date", lable[1,])
  
  coefficients$B <- -coefficients$ma1
  coefficients$A <- coefficients$ar1 + coefficients$ma1
  coefficients$AplusB <- coefficients$ar1
  
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
  
  #Plot Prediction
  df.pred <- cbind(df.date ,realoptweights$MSFT, omega$MSFT)
  colnames(df.pred) <- c("date", "realoptweights", "ARMAPrediction")
  df.pred$Naive <- optimalweights.uncond$MSFT
  
  plot.pred <- df.pred %>% 
    ggplot()+
    geom_line(aes(x = date, y = realoptweights), color = "#bababa")+
    geom_line(aes(x = date, y = ARMAPrediction), color = "#c41818")+
    geom_line(aes(x = date, y = Naive), color = "blue")
  
  
  #Plot Error
  plot.error <- epsilon %>% 
    ggplot()+
    geom_line(aes(x = date, y = MSFT), color = "#4f4f4f")
  
  acf(epsilon[stock])
  pacf(epsilon[stock])
  
  #Lijung Box test - Epsilon
  #Lijung Box test - weights
  LBtest.weights.pvalue <- NULL
  LBtest.weights.statistic <- NULL 
  for(i in 2:ncol(epsilon)){
    lb_i <- Box.test(epsilon[,i], lag = 10, type = "Ljung-Box")
    LBtest.weights.pvalue <- rbind(LBtest.weights.pvalue, lb_i$p.value) 
    LBtest.weights.statistic <- rbind(LBtest.weights.statistic, lb_i$statistic) 
  }
  LBtest <- cbind(LBtest.weights.statistic, LBtest.weights.pvalue)
  LBtest <- round(LBtest, 2)
  LBtest
  
  
  TO <- 1/nrow(omega) * sum(rowSums(abs(omega[-1])))
  
  PV <- 1/nrow(omega) * sum(rowSums(t(omega[-1]) * realoptweights[-1] * omega[-1]))
  DCW_weights <- omega[-1]
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
  portfolio.weight.random <- matrix(runif(40000), ncol=4)
  portfolio.weight.random <- portfolio.weight.random/rowSums(portfolio.weight.random)
  
  #unconditional 
  for(t in 1:nrow(return)){
    portfolio.var.return.naive[t,1] <- get.portfolio.variance(as.matrix(optimalweights.uncond), as.matrix(list.realCov.Date$R[,,t]))
    portfolio.var.return.naive[t,2] <- get.portfolio.mean.return(as.matrix(optimalweights.uncond), as.matrix(return[t,]))
    portfolio.var.return.dcw[t,1] <- get.portfolio.variance(as.matrix(portfoli.weights.DCw[t,]), as.matrix(list.realCov.Date$R[,,t]))
    portfolio.var.return.dcw[t,2] <- get.portfolio.mean.return(as.matrix(portfoli.weights.DCw[t,]), as.matrix(return[t,]))
  }
  colnames(portfolio.var.return.naive) <- c("Port.Variance", "Port.Return")
  portfolio.var.return.naive$Model <- "Naive"
  colnames(portfolio.var.return.dcw) <- c("Port.Variance", "Port.Return")
  portfolio.var.return.dcw$Model <- "DCW"
  portfolio.var.return <- rbind(cbind(df.date,portfolio.var.return.naive),cbind(df.date,portfolio.var.return.dcw))
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

#daily weighted average return over the entire time series and across all assets
matrix.weights <- as.matrix(realoptweights[-1])
matrix.weights <- as.matrix(omega[-1])
matrix.weights <- as.matrix(optimalweights.uncond)

portfolio.returns <- matrix.weights %*% t(return)

sum_t_i_weight_return <- 0
sum_t_i_weight <- 0
for(i in 1:ncol(matrix.weights)){
  for(t in 1:nrow(matrix.weights)){
    sum_t_i_weight_return = sum_t_i + abs(matrix.weights[[t,i]]) * return[t,i]
    sum_t_i_weight = sum_t_i + abs(matrix.weights[[t,i]])
  }
}
sum_t_i_weight_return / sum_t_i_weight

mean(portfolio.returns)
