#Dataframe with stock return
get.plot.return <- function(R, date, lable, asset = "MSFT"){
df.return <- cbind(R, date)
colnames(df.return) <- c(lable, "date")
df.return <- df.return %>% gather("stock", "return",-date)
df.return <- as.data.frame(df.return)
#Plot Stock return
plot.returns <- df.return %>% 
  filter(stock %in% c(asset)) %>% 
  ggplot(aes(x = date, y = return))+
  geom_line(color = "#020036")+
  labs(x ="Date", y = "Return")+
  scale_color_economist()+
  theme_economist()
return(plot.returns)
}

#Portfolio weigths from MSFT - Unrestricted
get.plot.omega <- function(omega, date, lable, asset = "MSFT"){
  df.realoptweights <- cbind(omega, date)
  colnames(df.realoptweights) <- c(lable, "date")
  df.realoptweights <- df.realoptweights %>% gather("stock", "weights",-date)
  df.realoptweights <- as.data.frame(df.realoptweights)
  plot.weigths <- df.realoptweights %>% filter(stock == asset) %>% 
    ggplot(aes(x = date, y = weights))+
    geom_line(color = "#020036")+
    labs(x ="Date", y = "Realized Optimal Portfolio Weights")+ 
    scale_color_economist()+
    theme_economist()
  return(plot.weigths)
}