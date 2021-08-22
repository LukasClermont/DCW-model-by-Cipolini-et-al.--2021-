#Descriptive Statistic 
get.Desc.Stat <- function(weights){
  df.descriptive.weights.Pos <- data.frame(min = min(weights),
                                           max = max(weights), 
                                           sd = sd(weights), 
                                           mean = mean(weights))
  return(round(df.descriptive.weights.Pos,4))
}
