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
param beta;

# Risk free rate for 1m
param r;

# Initial value risk factors
param value0RiskFactors;

# Initial value old contracts
param H0;

# Risk aversion
param lambda;

# Value risk factors unhedged
param valueRiskFactors {1..n};

# Initial prices of options
param price0Put {1..nPut};

# Prices per instrument and scenario
param priceLong {1..nForward,1..n};
param priceShort {1..nForward,1..n};
param pricePut {1..nPut,1..n};

# Initial Cash
param nu0 >= 0;

# Transaction costs
param costForward {1..nForward};
param costOption {1..nPut};

# Value of old contracts for each scenario
param H {1..n};

# Cash after hedge
var nu >= 0;


# UEBITDA hedged
var v {1..n} >= 0.00000001; #TODO


# Zeta at level beta
var zeta;

# Excess loss
var y {1..n} >= 0;

# Amounts
var alphaPut {1..nPut} >= 0;
var alphaLong {1..nForward} >= 0;
var alphaShort {1..nForward} >= 0;

# Objective function
minimize cvar: lambda * (zeta + (1 / (n * (1 - beta))) * sum {j in 1..n} (y[j])) - (1-lambda)/n * sum {i in 1..n} (log(v[i])) + sum {inst in 1..nForward} (kappaForward[inst]*costForward[inst]*(alphaLong[inst]+alphaShort[inst])) + sum {inst in 1..nPut} (kappaPut[inst]*costOption[inst]*alphaPut[inst]);

# Constraint on cash
subject to Cash:
nu = (exp(r/12))*(nu0 - sum {inst in 1..nPut} (price0Put[inst]*alphaPut[inst]) - sum {inst in 1..nForward} (costForward[inst]*(alphaLong[inst]+alphaShort[inst])) - sum {inst in 1..nPut} (costOption[inst]*alphaPut[inst]));
  
# Constraint on UEBITDA
subject to Uebitda {i in 1..n}:
v[i] = valueRiskFactors[i] + sum {inst in 1..nForward} (priceLong[inst,i]*alphaLong[inst] + priceShort[inst,i]*alphaShort[inst]) +  sum {inst in 1..nPut} (pricePut[inst,i]*alphaPut[inst]) + H[i] + nu;

# Constraint on yi
subject to yi {i in 1..n}:
y[i] >= (H0 + value0RiskFactors + nu0) - v[i] - zeta;
