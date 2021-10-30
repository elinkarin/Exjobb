# Optimization functions
library(rAMPL)

optimization <- function(objType, valueRiskFactors, priceLong, priceShort, kappaForward, kappaPut, costForward, nu0, r, H, value0RiskFactors, H0, beta, lambda, workingDirectory, pricePut=NULL, costOption=NULL, price0Put=NULL, solver=NULL, modelDirectory=NULL, amplDirectory=NULL) {
  
  env <- new(Environment, amplDirectory)
  
  ampl <- new(AMPL, env)

  if (!is.null(solver)) {
    ampl$setOption("solver", solver)
    ampl$setOption("ipopt_options", "constr_viol_tol=0.0000001")
  }
  
  # Read the model and data files.
  modelDirectory <- workingDirectory
    
  
  if (!is.null(pricePut)) {
    if (objType == "variance") {
      ampl$read(paste(modelDirectory, "/variance.mod", sep=""))
    }
    else if (objType == "cvar") {
      ampl$read(paste(modelDirectory, "/cvar.mod", sep=""))
    }
    else if (objType == "utility") {
      ampl$read(paste(modelDirectory, "/utility.mod", sep=""))
    }
    else {
      print("Wrong objective function")
    }
  }
  else {
    if (objType == "variance") {
      ampl$read(paste(modelDirectory, "/varianceNoPut.mod", sep=""))
    }
    else if (objType == "cvar") {
      ampl$read(paste(modelDirectory, "/cvarNoPut.mod", sep=""))
    }
    else if (objType == "utility") {
      ampl$read(paste(modelDirectory, "/utilityNoPut.mod", sep=""))
    }
    else {
      print("Wrong objective function")
    }
  }
  
  n <- length(valueRiskFactors)
  nForward <- dim(priceLong)[1]
  
  nAMPL <- ampl$getParameter("n")
  nAMPL$setValues(n)
  nForwardAMPL <- ampl$getParameter("nForward")
  nForwardAMPL$setValues(nForward)
  kappaForwardAMPL <- ampl$getParameter("kappaForward")
  kappaForwardAMPL$setValues(kappaForward)
  rAMPL <- ampl$getParameter("r")
  rAMPL$setValues(r)
  nu0AMPL <- ampl$getParameter("nu0")
  nu0AMPL$setValues(nu0)
  valueRiskFactorsAMPL <- ampl$getParameter("valueRiskFactors")
  valueRiskFactorsAMPL$setValues(valueRiskFactors)
  costForwardAMPL <- ampl$getParameter("costForward")
  costForwardAMPL$setValues(costForward)
  HAMPL <- ampl$getParameter("H")
  HAMPL$setValues(H)
  H0AMPL <- ampl$getParameter("H0")
  H0AMPL$setValues(H0)
  value0RiskFactorsAMPL <- ampl$getParameter("value0RiskFactors")
  value0RiskFactorsAMPL$setValues(value0RiskFactors)
  
  priceLongAMPL <- ampl$getParameter("priceLong")
  priceShortAMPL <- ampl$getParameter("priceShort")
  pricesLong <- numeric(n*nForward)
  pricesShort <- numeric(n*nForward)
  nScenarios <- numeric(n*nForward)
  nForwardScenario <- numeric(n*nForward)
  
  for (j in 1:nForward) {
    for (i in 1:n) {
      pricesLong[(j-1)*n+i] <- priceLong[j,i]
      pricesShort[(j-1)*n+i] <- priceShort[j,i]
      nScenarios[(j-1)*n+i] <- c(i)
      nForwardScenario[(j-1)*n+i] <- c(j)
    }
  }
  
  priceLongAMPL$setValues(data.frame(nForwardScenario, nScenarios, pricesLong))
  priceShortAMPL$setValues(data.frame(nForwardScenario, nScenarios, pricesShort))
  
  if (!is.null(pricePut)) {
    nPut <- dim(pricePut)[1]
    
    nPutAMPL <- ampl$getParameter("nPut")
    nPutAMPL$setValues(nPut)
    kappaPutAMPL <- ampl$getParameter("kappaPut")
    kappaPutAMPL$setValues(kappaPut)
    price0PutAMPL <- ampl$getParameter("price0Put")
    price0PutAMPL$setValues(price0Put)
    costOptionAMPL <- ampl$getParameter("costOption")
    costOptionAMPL$setValues(costOption)
    
    pricePutAMPL <- ampl$getParameter("pricePut")
    pricesPut <- numeric(n*nPut)
    nScenarios <- numeric(n*nPut)
    nPutScenario <- numeric(n*nPut)
    
    for (j in 1:nPut) {
      for (i in 1:n) {
        pricesPut[(j-1)*n+i] <- pricePut[j,i]
        nScenarios[(j-1)*n+i] <- c(i)
        nPutScenario[(j-1)*n+i] <- c(j)
      }
    }
    pricePutAMPL$setValues(data.frame(nPutScenario, nScenarios, pricesPut))
  }
  
  if (objType == "cvar") {
    betaAMPL <- ampl$getParameter("beta")
    betaAMPL$setValues(beta)
  }
  else if (objType == "utility") {
    betaAMPL <- ampl$getParameter("beta")
    betaAMPL$setValues(beta)
    lambdaAMPL <- ampl$getParameter("lambda")
    lambdaAMPL$setValues(lambda)
  }
  
  ampl$solve()
  
  # Get the values of the variable Buy in a dataframe object
  alphaLong <- ampl$getVariable("alphaLong")
  alphaShort <- ampl$getVariable("alphaShort")
  dfLong <- alphaLong$getValues()
  dfShort <- alphaShort$getValues()
  nuAMPL <- ampl$getVariable("nu")
  dfNu <- nuAMPL$getValues()
  
  df <- merge(dfLong, dfShort, by="index0", all.x=TRUE, all.y=TRUE)
  
  if (!is.null(pricePut)) {
    alphaPut <- ampl$getVariable("alphaPut")
    dfPut <- alphaPut$getValues()
    df <- merge(dfPut, df, by="index0", all.x=TRUE, all.y=TRUE)
  }
  
  # Combine date, then return it
  dfList <- list(df,dfNu)
  
  return(dfList)
}
