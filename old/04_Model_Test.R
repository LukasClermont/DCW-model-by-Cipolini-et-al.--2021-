library(PortfolioAnalytics)

data(edhec)
returns <- edhec[, 1:4]
colnames(returns) <- c("CA", "CTAG", "DS", "EM")
print(head(returns, 5))
fund.names <- colnames(returns)
pspec <- portfolio.spec(assets=fund.names)
print.default(pspec)
pspec <- add.constraint(portfolio=pspec,
                        type="weight_sum",
                        min_sum=1,
                        max_sum=1)
pspec <- add.objective(portfolio=pspec,
                       type='risk',
                       name='ETL',
                       arguments=list(p=0.95))
