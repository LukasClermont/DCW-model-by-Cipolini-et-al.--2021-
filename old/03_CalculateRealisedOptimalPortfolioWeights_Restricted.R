#Calculate realized optimal portfolio weights: No-short-Portfolio
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


#Descriptive Statistic Postive vs all
df.descriptive.weights.Pos <- NULL
df.descriptive.weights.Pos$min <- min(realoptweights.positive[-1])
df.descriptive.weights.Pos$max <- max(realoptweights.positive[-1]) 
df.descriptive.weights.Pos$sd <- sd(as.matrix(realoptweights.positive[-1])) 
df.descriptive.weights.Pos$mean <- mean(as.matrix(realoptweights.positive[-1])) 
df.descriptive.weights.Pos

df.descriptive.weights.all <- NULL
df.descriptive.weights.all$min <- min(realoptweights[-1])
df.descriptive.weights.all$max <- max(realoptweights[-1]) 
df.descriptive.weights.all$sd <- sd(as.matrix(realoptweights[-1])) 
df.descriptive.weights.all$mean <- mean(as.matrix(realoptweights[-1])) 
count(df.descriptive.weights.all)
df.descriptive.weights.all
