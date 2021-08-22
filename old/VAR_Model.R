library(R.matlab)
library(tidyverse)
library(vars)

return_computer <- readMat("return_computer.mat")
df.return <- as.data.frame(return_computer$r)
df.return <- rowid_to_column(df.return, "Date")
#df.return <- gather(df.return, "ID", "return", -Date)

#Plot
ggplot(df.return) + geom_point(aes(x = V1, y = V2))

attach(df.return)
agri <- cbind(diff(V1), diff(V2))
ts.v1 <- ts(V1)
ts.v2 <- ts(V2)

OLS <- lm(ts.v1 ~ ts.v2)
summary(OLS)

#Determine the persistance of the model
acf(ts.v1)
pacf(ts.v1)
acf(ts.v2)
pacf(ts.v2)

#Finding optimal lags
okan.bv <- cbind(ts.v1, ts.v2)
colnames(okan.bv) <- c("Aktie1", "Aktie2")

lagselect <- VARselect(okan.bv, lag.max = 10, type = "const")
lagselect$selection

#Building VAR
Model1 <- VAR(okan.bv, p=4, type = "const", season = NULL, exog = NULL)

