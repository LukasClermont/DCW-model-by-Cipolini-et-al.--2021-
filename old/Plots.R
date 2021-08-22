#Dataframe with stock return
df.return <- cbind(data$R, data$date)
colnames(df.return) <- c(data$lable, "date")
df.return <- df.return %>% gather("stock", "return",-date)
df.return <- as.data.frame(df.return)
#Plot Stock return
plot.returns.MSFT <- df.return %>% 
  filter(stock %in% c("MSFT")) %>% 
  ggplot(aes(x = date, y = return))+
  geom_line(color = "#020036")+
  labs(x ="Date", y = "Return")+
  scale_color_economist()+
  theme_economist()

#Portfolio weigths from MSFT - Unrestricted
df.realoptweights <- cbind(omega$real.omega.norest, data$date)
colnames(df.realoptweights) <- c(data$lable, "date")
df.realoptweights <- df.realoptweights %>% gather("stock", "weights",-date)
df.realoptweights <- as.data.frame(df.realoptweights)
plot.weigths.MSFT <- df.realoptweights %>% filter(stock == "MSFT") %>% 
  ggplot(aes(x = date, y = weights))+
  geom_line(color = "#020036")+
  labs(x ="Date", y = "Realized Optimal Portfolio Weights")+ 
  scale_color_economist()+
  theme_economist()