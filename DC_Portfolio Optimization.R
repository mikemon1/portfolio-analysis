
library(xts)
library(ggplot2)
library(quadprog)

# Load the package
library(PortfolioAnalytics)

# Load the data
data(indexes)

# Subset the data
index_returns <- indexes[ ,c(1:4)]

# Print the head of the data
head(index_returns)

# Create the portfolio specification
port_spec <- portfolio.spec(colnames(index_returns))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Solve the optimization problem
opt <- optimize.portfolio(index_returns, portfolio = port_spec, optimize_method= "random")

# Print the results of the optimization
print(opt)

# Extract the optimal weights
extractWeights(opt)

# Extract the objective measures for the single period optimization
extractObjectiveMeasures(opt)

# Chart the optimal weights
chart.Weights(opt)

# Create the portfolio specification
port_spec <- portfolio.spec(assets = colnames(index_returns))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to maximize portfolio mean return
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")


# Add an objective to minimize portfolio variance
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "var", risk_aversion = 10)

# Solve the optimization problem
opt <- optimize.portfolio(R = index_returns, portfolio = port_spec, optimize_method = "random")

## Backtesting

# Run a single period optimization using random portfolios as the optimization method
opt <- optimize.portfolio(R = asset_returns,
                          portfolio = port_spec,
                          optimize_method = "random",
                          rp = rp, trace = TRUE)

# Print the output of the single-period optimization
print(opt)

# Run the optimization backtest with quarterly rebalancing
opt_rebal <- optimize.portfolio.rebalancing(R = asset_returns,
                                            portfolio = port_spec,
                                            optimize_method = "random", rp = rp,
                                            trace = TRUE, search_size = 1000,
                                            rebalance_on = "quarters",
                                            training_period = 60,
                                            rolling_window = 60)


# Print the output of the optimization backtest
print(opt_rebal)

#####################################
## User defined Objective Functions #
#####################################

# Annualized Sharpe Ratio Function
sr_annualized <- function(R,weights,sigma,scale, rfr){
  # Geometric annualized return
  r <- Return.annualized(Return.portfolio(R, weights), scale = scale)
  # Annualized excess return
  re <- r-rfr
  
  # Annualized portfolio StdDev
  pasd <- sqrt(as.numeric(t(weights) %*%
                            sigma %*% weights)) * sqrt(scale)
  
  return(re / pasd)
}

data(edhec)
asset_returns <- edhec[,1:4]
port_spec <- portfolio.spec(assets = colnames(asset_returns))
port_spec <- add.constraint(portfolio = port_spec,
                            type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
port_spec <- add.objective(portfolio = port_spec,
                            type = "return", name = "sr_annualized",
                            arguments = list(scale = 12, rfr = 0.02))

###########################
#### Case Study Example ###
###########################
library(PerformanceAnalytics)
data(edhec)

returns <- edhec
# Create an Equal Weight Benchmark
n <- ncol(returns)
equal_weights <- rep(1/n , n)

benchmark_returns <- Return.portfolio(R = returns, weights = equal_weights,
                                      rebalance_on = "quarters")
colnames(benchmark_returns) <- "benchmark"

# Benchmark Performance
table.AnnualizedReturns(benchmark_returns)

# Base portfolio specification

base_port_spec <- portfolio.spec(assets = colnames(returns))
base_port_spec <- add.constraint(portfolio = base_port_spec, type = "full_investment")
base_port_spec <- add.constraint(portfolio = base_port_spec, type = "long_only")
base_port_spec <- add.objective(portfolio = base_port_spec, type = "risk", name = "StdDev")

box_port_spec <- base_port_spec
# Update a constraint
box_port_spec <- add.constraint(portfolio = box_port_spec, 
                                type = "box",
                                min = 0.05, max = 0.4,
                                indexnum = 2)

# Backtesting
opt_base <- optimize.portfolio.rebalancing(R = returns,
                                           optimize_method = "ROI",
                                           portfolio = base_port_spec,
                                           rebalance_on = "quarters",
                                           training_period = 60,
                                           rolling_window = 60)

# Backtesting
opt_box <- optimize.portfolio.rebalancing(R = returns,
                                           optimize_method = "ROI",
                                           portfolio = box_port_spec,
                                           rebalance_on = "quarters",
                                           training_period = 60,
                                           rolling_window = 60)

# Calculate portfolio returns
base_returns <- Return.portfolio(returns, extractWeights(opt_base))
colnames(base_returns) <- "base"
box_returns <- Return.portfolio(returns, extractWeights(opt_box))
colnames(box_returns) <- "base"