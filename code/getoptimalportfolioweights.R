get.realized.optimal.portfolio.weights <- function(covmatix){
  jota <- vec.unity(nrow(covmatix))
  rel.opt.port.wei <- 1/(jota %*% solve(covmatix) %*% t(jota))[[1]] * solve(covmatix) %*% t(jota)
  return(t(rel.opt.port.wei))
}

get.realized.optimal.portfolio.weights.positive <- function(covmatrix){
  d_vector <- rep(0, 30)
  A_matrix <- cbind(rep(1, 30), diag(30))
  b_vector <- c(1, rep(0, 30))
  D_matrix <- 2 * covmatrix
  
  realoptweights.positive <- solve.QP(Dmat = D_matrix, dvec = d_vector, Amat = A_matrix, bvec = b_vector, meq = 1)
  realoptweights.positive <- t(realoptweights.positive$solution)
  realoptweights.positive <- round(realoptweights.positive, 10)
  return(realoptweights.positive)
}