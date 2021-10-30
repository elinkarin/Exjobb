# Number of scenarios
param n;

# Number of put instruments
param nPut;
# Number of forward instruments
param nForward;

# Sizing parameter
param kappaForward {1..nForward};
param kappaPut {1..nPut};

# Level
#param beta;

# Risk free rate for 1m
param r;

# Initial value risk factors
param value0RiskFactors;

# Initial value old contracts
param H0;

# Value risk factors unhedged
param valueRiskFactors {1..n};

# Initial prices of options
param price0Put {1..nPut};

# Prices per instrument and scenario
param priceLong {1..nForward,1..n};
param priceShort {1..nForward,1..n};
param pricePut {1..nPut,1..n};

# Initial Cash
param nu0;

# Transaction costs
param costForward {1..nForward};
param costOption {1..nPut};

# Value of old contracts for each scenario
param H {1..n};

# Cash after hedge
var nu >= 0;

# Effect on portfolio when hedged
var v {1..n};

# Mean
var mu;

# Amounts
var alphaPut {1..nPut} >= 0;
var alphaLong {1..nForward} >= 0;
var alphaShort {1..nForward} >= 0;


# Objective function
minimize Variance: (1/(n-1)) * sum {i in 1..n} (((v[i]) - mu)^2) + sum {inst in 1..nForward} (kappaForward[inst]*costForward[inst]*(alphaLong[inst]+alphaShort[inst])) + sum {inst in 1..nPut} (kappaPut[inst]*costOption[inst]*alphaPut[inst]);

# Constraint on cash
subject to Cash:
nu = (exp(r/12))*(nu0 - sum {inst in 1..nPut} (price0Put[inst]*alphaPut[inst]) - sum {inst in 1..nForward} (costForward[inst]*(alphaLong[inst]+alphaShort[inst])) - sum {inst in 1..nPut} (costOption[inst]*alphaPut[inst]));
  
# Constraint on portfolio value
subject to Uebitda {i in 1..n}:
v[i] = valueRiskFactors[i] + sum {inst in 1..nForward} (priceLong[inst,i]*alphaLong[inst] + priceShort[inst,i]*alphaShort[inst]) +  sum {inst in 1..nPut} (pricePut[inst,i]*alphaPut[inst]) + H[i] + nu;

# Constraint calc mean
subject to Mean:
mu = (1/n) * sum {i in 1..n} (v[i]);
