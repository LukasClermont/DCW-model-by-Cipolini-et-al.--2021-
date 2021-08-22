
library(rmgarch)
# univariate normal GARCH(1,1) for each series
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,1), 
                                                model = "sGARCH"), 
                          distribution.model = "norm")

# dcc specification - GARCH(1,1) for conditional correlations
dcc.garch11.spec = dccspec(uspec = multispec( replicate(30, garch11.spec) ), 
                           dccOrder = c(1,1), 
                           distribution = "mvnorm")

dcc.fit = dccfit(dcc.garch11.spec, data = return[1:253,])


dcc.fcst = dccforecast(dcc.fit, n.ahead=30)
weight.dcc <- as.data.frame(dcc.fcst@mforecast$mu)
get.portfolio.variance(as.matrix(weight.dcc[10,]), as.matrix(list.realCov.Date$R[,,263]))
sum(weight.dcc[10,])

