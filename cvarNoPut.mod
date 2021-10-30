# Number of scenarios
param n;

# Number of forward instruments
param nForward;

# Sizing parameter
param kappaForward {1..nForward};

# Level
param beta;

# Risk free rate for 1m
param r;

# Initial value risk factors
param value0RiskFactors;

# Initial value old contracts
param H0;

# Value risk factors unhedged
param valueRiskFactors {1..n};

# Prices per instrument and scenario
param priceLong {1..nForward,1..n};
param priceShort {1..nForward,1..n};

# Initial Cash
param nu0 >= 0;

# Transaction costs
param costForward {1..nForward};

# Value of old contracts for each scenario
param H {1..n};

# Cash after hedge
var nu >= 0;

# Effect on portfolio when hedged
var v {1..n};

# Zeta at level beta
var zeta;

# Excess loss
var y {1..n} >= 0;

# Amounts
var alphaLong {1..nForward} >= 0;
var alphaShort {1..nForward} >= 0;

# Objective function, TODO: check if n is right
minimize cvar: zeta + (1 / (n * (1 - beta))) * sum {j in 1..n} (y[j]) + sum {inst in 1..nForward} (kappaForward[inst]*costForward[inst]*(alphaLong[inst]+alphaShort[inst]));

# Constraint on cash
subject to Cash:
nu = (exp(r/12))*(nu0 - sum {inst in 1..nForward} (costForward[inst]*(alphaLong[inst]+alphaShort[inst])));
  
# Constraint on UEBITDA
subject to Uebitda {i in 1..n}:
v[i] = valueRiskFactors[i] + sum {inst in 1..nForward} (priceLong[inst,i]*alphaLong[inst] + priceShort[inst,i]*alphaShort[inst]) + H[i] + nu;
  
# Constraint on yi
subject to yi {i in 1..n}:
y[i] >= (H0 + value0RiskFactors + nu0) - v[i] - zeta;

    
    
    