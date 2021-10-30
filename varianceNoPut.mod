# UEBITDA, prices, kappa, transactionCost, nu0, BSc0, BSp0, r

# Number of scenarios
param n;

# Number of forward instruments = 77
param nForward;

# Sizing parameter
param kappaForward {1..nForward};

# Risk free rate for 1m NOK
param r;

# Value of the risk factors
param valueRiskFactors {1..n};

param priceLong {1..nForward,1..n};
param priceShort {1..nForward,1..n};

# Initial Cash
param nu0;

# Initial value risk factors
param value0RiskFactors;

# Initial value old contracts
param H0;

# Transaction costs
param costForward {1..nForward};

# Value of old contracts for each scenario
param H {1..n};

# Cash after hedge
var nu >= 0;

# Value of portfolio with hedge
var v {1..n};

# Mean
var mu;

# Amount to buy of each contract
var alphaLong {1..nForward} >= 0;
var alphaShort {1..nForward} >= 0;


# Objective function
minimize Variance: (1/n) * sum {i in 1..n} (((v[i]) - mu)^2) + sum {inst in 1..nForward} (kappaForward[inst]*costForward[inst]*(alphaLong[inst]+alphaShort[inst]));


# Constraint on cash
subject to Cash:
nu = (exp(r/12))*(nu0 - sum {inst in 1..nForward} (costForward[inst]*(alphaLong[inst]+alphaShort[inst])));
  
# Constraint on portfolio value
subject to Uebitda {i in 1..n}:
v[i] = valueRiskFactors[i] + sum {inst in 1..nForward} (priceLong[inst,i]*alphaLong[inst] + priceShort[inst,i]*alphaShort[inst]) + H[i] + nu;
  # +  sum {inst in 1..nPut} (pricePut[inst,i]*alphaPut[inst])
  
# Constraint calc mean
subject to Mean:
mu = (1/n) * sum {i in 1..n} (v[i]);
      
      