library(R.matlab)
library(ggplot2)
library(tidyverse)

return_computer <- readMat("return_computer.mat")
Cluster_30D_realcov_cleaner_liquiditysorting <- readMat("Cluster_30D_realcov_cleaner_liquiditysorting.mat")
R <- Cluster_30D_realcov_cleaner_liquiditysorting[["R"]]

return <- as.data.frame(return_computer$r)
return <- rowid_to_column(return, "Date")
return <- gather(return, "ID", "return", -Date)


ggplot(data = return, aes(x = Date, y = return, color = ID)) +
  geom_line()

#realized optimal portfolio weights
vec.unity <- function(lenghts){
  return(rep(1, lenghts))
}
unity.vector <- t(vec.unity(ncol(cov)))

for(i in 1:3272){
  cov <- Cluster_30D_realcov_cleaner_liquiditysorting$R[,,i]
  optimalweights_i <- solve(unity.vector %*% solve(cov) %*% t(unity.vector))[[1]] * solve(cov) %*% t(unity.vector)
  optimalweights <- rbind(optimalweights, t(optimalweights_i))
}
optimalweights <- rowid_to_column(optimalweights, "Date")

ggplot(optimalweights, aes(x = Date, y = V2))+
  geom_line()
