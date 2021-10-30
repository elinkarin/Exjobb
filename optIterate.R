opt <- function(workingDirectory, n, D, commission, tau, tauIVAluminium, tauIVUSDNOK, tauOIS, tauFuture, exposure, kappa, lambda, nu0, cash, beta, moneynessAluminium, deltaUSDNOK, today, startDate, oldContracts, optType, p_list, type, instrument, months, saveCash, percentExposure, amplDir, dataFile=NULL) {
  
  # Retrieve data from Excel ---------------------------------------------------
  
  # If no data file is given, use test data
  if(is.null(dataFile)) {
    dataName <- "data.xlsx"
  }
  else {
    dataName <- as.character(dataFile["datapath"])
  }
  
  # OIS
  dataOISEUR <- read_excel(dataName, sheet = "OIS_EUR")
  dataOISUSD <- read_excel(dataName, sheet = "OIS_USD")
  dataOISNOK <- read_excel(dataName, sheet = "OIS_NOK")
  dataSwapBRL <- read_excel(dataName, sheet = "BRL_USD_swap")
  
  # Implicit volatility
  allDataIVAluminium <- read_excel(dataName, sheet = "IV_ALUMINIUM")
  allDataIVUSDNOK <- read_excel(dataName, sheet = "IV_USDNOK")
  
  # Prices futures aluminium
  allMarketPricesFuture <- read_excel(dataName, sheet = "FUTURES_ALUMINIUM")
  
  # Simulations
  simulations <- read_excel(dataName, sheet = "SIMULATIONS")
  
  # Old contracts
  if (today == startDate) {
    oldContracts[[1]] <- data.frame(read_excel(dataName, sheet = "OPTION_PUT"))
    oldContracts[[2]] <- data.frame(read_excel(dataName, sheet = "FORWARD_LONG"))
    oldContracts[[3]] <- data.frame(read_excel(dataName, sheet = "FORWARD_SHORT"))
    oldContracts[[1]][["startDate"]] <- as.numeric(as.Date(oldContracts[[1]][["startDate"]]))
    oldContracts[[2]][["startDate"]] <- as.numeric(as.Date(oldContracts[[2]][["startDate"]]))
    oldContracts[[3]][["startDate"]] <- as.numeric(as.Date(oldContracts[[3]][["startDate"]]))
  }
  
  # Scenario generation --------------------------------------------------------
  
  if(dim(simulations)[1] == 0) {
    # Load parameters from Erik and Victor
    load("param_scenario_generation.RData")
    
    # Load today's data
    dataD <- read.xlsx("data.xlsx", sheet = "DATA_DAILY", detectDates = TRUE)
    dataMr <- read.xlsx("data.xlsx", sheet = "DATA_MONTHLY", detectDates = TRUE)

    # Make data monthly/quarterly and merge and logtransform
    dataM <- merge(monthdata(dataD), dataMr, by='Timestamp', all.x=T)
    dataM[,-1] <- log(dataM[,-1])
    
    # Suppress warnings
    oldw <- getOption("warn")
    options(warn = -1)
    
    # Scenario generation only works for the first day of the month
    monthStart <- as.POSIXlt(today)
    monthStart$mday <- 1
    monthStart <- as.Date(monthStart)

    # Generate scenarios
    allPrices <- price_simulation(paramlist=p_list, types=type, dt=1/12, 
                                  startprices=dataM[match(monthStart,dataM$Timestamp), c(1:8,16:19)], 
                                  steps=2, n_times=n) 
    
    # Enable warnings again
    options(warn = oldw)
    
    # Save generated prices as MNOK
    pricesAluminium <- allPrices$LME3M[1:n+1] * allPrices$USDNOKMSCI[1:n+1]/(10^6)
    pricesIngot <- allPrices$INGOT[1:n+1] * allPrices$USDNOKMSCI[1:n+1]/(10^6)
    pricesAlumina <- allPrices$PAX[1:n+1] * allPrices$USDNOKMSCI[1:n+1]/(10^6)
    pricesCoke <- allPrices$COKE[1:n+1] * allPrices$USDNOKMSCI[1:n+1]/(10^6)
    pricesOil <- allPrices$FUELOIL[1:n+1] * allPrices$USDNOKMSCI[1:n+1]/(10^6)
    pricesCaustic <- allPrices$CAUSTIC[1:n+1] * allPrices$USDNOKMSCI[1:n+1]/(10^6)
    pricesPitch <- allPrices$PITCH[1:n+1] * allPrices$EURNOK[1:n+1]/(10^6)
    pricesCoal <- allPrices$COAL[1:n+1] * allPrices$USDNOKMSCI[1:n+1]/(10^6)
    pricesUSDNOK <- allPrices$USDNOKMSCI[1:n+1]/(10^6)
    pricesBRLNOK <- allPrices$BRLNOK[1:n+1]/(10^6)
    pricesEURNOK <- allPrices$EURNOK[1:n+1]/(10^6)
  }
  else{
    pricesUSDNOK <- data.frame(rbind(simulations$spotUSDNOK, simulations$simUSDNOK)/(10^6))
    pricesBRLNOK <- data.frame(rbind(simulations$spotBRLNOK, simulations$simBRLNOK)/(10^6))
    pricesEURNOK <- data.frame(rbind(simulations$spotEURNOK, simulations$simEURNOK)/(10^6))
    pricesAluminium <- data.frame(rbind(simulations$spotAluminium, simulations$simAluminium) * pricesUSDNOK/(10^6))
    pricesIngot <- data.frame(rbind(simulations$spotIngot, simulations$simIngot) * pricesUSDNOK/(10^6))
    pricesAlumina <- data.frame(rbind(simulations$spotAlumina, simulations$simAlumina) * pricesUSDNOK/(10^6))
    pricesCoke <- data.frame(rbind(simulations$spotCoke, simulations$simCoke) * pricesUSDNOK/(10^6))
    pricesOil <- data.frame(rbind(simulations$spotOil, simulations$simOil) * pricesUSDNOK/(10^6))
    pricesCaustic <- data.frame(rbind(simulations$spotCaustic, simulations$simCaustic) * pricesUSDNOK/(10^6))
    pricesPitch <- data.frame(rbind(simulations$spotPitch, simulations$simPitch) * pricesEURNOK/(10^6))
    pricesCoal <- data.frame(rbind(simulations$spotCoal, simulations$simCoal) * pricesUSDNOK/(10^6))
  }
  
  # Create matrix of all scenarios, size 11xn 
  pricesRiskFactors <- rbind(pricesAluminium[2,], pricesIngot[2,],
                             pricesAlumina[2,], pricesCoke[2,], pricesOil[2,],
                             pricesCaustic[2,], pricesPitch[2,], pricesCoal[2,],
                             pricesUSDNOK[2,], pricesBRLNOK[2,],
                             pricesEURNOK[2,])
  
  
  # Create vector of all spot prices
  spot <- c(pricesAluminium[1,1], pricesIngot[1,1], pricesAlumina[1,1],
            pricesCoke[1,1], pricesOil[1,1], pricesCaustic[1,1],
            pricesPitch[1,1], pricesCoal[1,1], pricesUSDNOK[1,1],
            pricesBRLNOK[1,1], pricesEURNOK[1,1])
  
  
  # Enable different combinations of risk factors ------------------------------
  
  # Create variables
  indexForward <- numeric(0)
  indexPut <- numeric(0)
  startIndexForward <- numeric(0)
  kappaForward <- numeric(0)
  kappaPut <- numeric(0)
  spotForward <- numeric(0)
  spotPut <- numeric(0)
  exposureInstruments <- numeric(11)

  # Add values to variables if risk factor is chosen
  if ("aluminium" %in% instrument) {
    startIndexPut <- 1
    indexForward <- c(indexForward, c(1:7))
    indexPut <- c(indexPut, c(1:63))
    startIndexForward <- c(startIndexForward, c(1))
    kappaPut <- c(kappaPut, rep(c(kappa[1]),times=63))
    spotPut <- c(spotPut, rep(c(spot[1]),times=63))
    kappaForward <- c(kappaForward, rep(c(kappa[1]),times=7))
    spotForward <- c(spotForward, rep(c(spot[1]),times=7))
    exposureInstruments[1] <- exposure[1]
  }
  if ("ingot" %in% instrument) {
    indexForward <- c(indexForward, c(8:14))
    startIndexForward <- c(startIndexForward, c(8))
    kappaForward <- c(kappaForward, rep(c(kappa[2]),times=7))
    spotForward <- c(spotForward, rep(c(spot[2]),times=7))
    exposureInstruments[2] <- exposure[2]
  }
  if ("alumina" %in% instrument) {
    indexForward <- c(indexForward, c(15:21))
    startIndexForward <- c(startIndexForward, c(15))
    kappaForward <- c(kappaForward, rep(c(kappa[3]),times=7))
    spotForward <- c(spotForward, rep(c(spot[3]),times=7))
    exposureInstruments[3] <- exposure[3]
  }
  if ("coke" %in% instrument) {
    indexForward <- c(indexForward, c(22:28))
    startIndexForward <- c(startIndexForward, c(22))
    kappaForward <- c(kappaForward, rep(c(kappa[4]),times=7))
    spotForward <- c(spotForward, rep(c(spot[4]),times=7))
    exposureInstruments[4] <- exposure[4]
  }
  if ("oil" %in% instrument) {
    indexForward <- c(indexForward, c(29:35))
    startIndexForward <- c(startIndexForward, c(29))
    kappaForward <- c(kappaForward, rep(c(kappa[5]),times=7))
    spotForward <- c(spotForward, rep(c(spot[5]),times=7))
    exposureInstruments[5] <- exposure[5]
  }
  if ("caustic" %in% instrument) {
    indexForward <- c(indexForward, c(36:42))
    startIndexForward <- c(startIndexForward, c(36))
    kappaForward <- c(kappaForward, rep(c(kappa[6]),times=7))
    spotForward <- c(spotForward, rep(c(spot[6]),times=7))
    exposureInstruments[6] <- exposure[6]
  }
  if ("pitch" %in% instrument) {
    indexForward <- c(indexForward, c(43:49))
    startIndexForward <- c(startIndexForward, c(43))
    kappaForward <- c(kappaForward, rep(c(kappa[7]),times=7))
    spotForward <- c(spotForward, rep(c(spot[7]),times=7))
    exposureInstruments[7] <- exposure[7]
  }
  if ("coal" %in% instrument) {
    indexForward <- c(indexForward, c(50:56))
    startIndexForward <- c(startIndexForward, c(50))
    kappaForward <- c(kappaForward, rep(c(kappa[8]),times=7))
    spotForward <- c(spotForward, rep(c(spot[8]),times=7))
    exposureInstruments[8] <- exposure[8]
  }
  if ("usdnok" %in% instrument) {
    if (!("aluminium" %in% instrument)) {
      startIndexPut <- 64
    }
    indexPut <- c(indexPut, c(64:119))
    indexForward <- c(indexForward, c(57:63))
    startIndexForward <- c(startIndexForward, c(57))
    kappaPut <- c(kappaPut, rep(c(kappa[9]),times=56))
    spotPut <- c(spotPut, rep(c(spot[9]),times=56))
    kappaForward <- c(kappaForward, rep(c(kappa[9]),times=7))
    spotForward <- c(spotForward, rep(c(spot[9]),times=7))
    exposureInstruments[9] <- exposure[9]
  }
  if ("brlnok" %in% instrument) {
    indexForward <- c(indexForward, c(64:70))
    startIndexForward <- c(startIndexForward, c(64))
    kappaForward <- c(kappaForward, rep(c(kappa[10]),times=7))
    spotForward <- c(spotForward, rep(c(spot[10]),times=7))
    exposureInstruments[10] <- exposure[10]
  }
  if ("eurnok" %in% instrument) {
    indexForward <- c(indexForward, c(71:77))
    startIndexForward <- c(startIndexForward, c(71))
    kappaForward <- c(kappaForward, rep(c(kappa[11]),times=7))
    spotForward <- c(spotForward, rep(c(spot[11]),times=7))
    exposureInstruments[11] <- exposure[11]
  }
  alphaStopForward <- length(indexForward)
  alphaStopPut <- length(indexPut)
  
  
  # Risk free rate -------------------------------------------------------------
  
  # Find closest available date in data and save that data
  # If no data available, use the closest earlier date available
  todayOISEUR <- findClosestAvailableDate(today, dataOISEUR, "backward")[,2:16]
  todayOISUSD <- findClosestAvailableDate(today, dataOISUSD, "backward")[,2:16]
  todayOISNOK <- findClosestAvailableDate(today, dataOISNOK, "forward")[,2:16]
  todaySwapBRL <- findClosestAvailableDate(today, dataSwapBRL, "backward")[,2:13]
  
  # Assign constant time to maturities for USDBRL swap
  tauSwapBRL <- c(1/12, 2/12, 3/12, 6/12, 9/12, 1, 15/12, 18/12, 2, 3, 4)
  
  # Calculate risk free rate
  rEUR <- OIStoR(tauOIS, as.numeric(todayOISEUR), D)
  rNOK <- OIStoR(tauOIS, as.numeric(todayOISNOK), D)
  rUSD <- OIStoR(tauOIS, as.numeric(todayOISUSD), D)
  rBRL <- swapToRate(tauSwapBRL, tauOIS, as.numeric(todaySwapBRL), rUSD, D)
  
  # Implicit volatility, sigma -------------------------------------------------
  
  # Find closest available date in data and save that data
  # If no data available, use the closest earlier date available
  todayIVAluminium <- findClosestAvailableDate(today, allDataIVAluminium, "backward")[,2:(length(tauIVAluminium)*length(moneynessAluminium)+1)]
  if (today >= as.Date("2017-11-30")) {
    todayIVUSDNOK <- findClosestAvailableDate(today, allDataIVUSDNOK, "backward")[,2:(length(tauIVUSDNOK)*length(deltaUSDNOK)+1)]
  }
  else {
    todayIVUSDNOK <- findClosestAvailableDate(today, allDataIVUSDNOK, "forward")[,2:(length(tauIVUSDNOK)*length(deltaUSDNOK)+1)]
  }
  
  # Save data as matrix
  dataIVAluminium <- matrixIV(moneynessAluminium, todayIVAluminium, tauIVAluminium)
  dataIVUSDNOK <- matrixIV(deltaUSDNOK, todayIVUSDNOK, tauIVUSDNOK)
  for (i in 1:(dim(dataIVUSDNOK))[1]) {
    dataIVUSDNOK[i,2] <- deltaToStrikeForPut(dataIVUSDNOK[i,2],
                                             dataIVUSDNOK[i,3],
                                             dataIVUSDNOK[i,1])
  }
  
  # Calculate moneyness from delta for USDNOK
  moneynessUSDNOK <- numeric(dim(dataIVUSDNOK)[1])
  for (index in 1:dim(dataIVUSDNOK)[1]) {
    moneynessUSDNOK[index] <- unlist(dataIVUSDNOK[index,2])
  }

  # Calculate volatility
  sigmaAluminium <- interpolateMatrix(tauIVAluminium, D, dataIVAluminium)
  sigmaUSDNOK <- interpolateMatrix(tauIVUSDNOK, D, dataIVUSDNOK)
  
  # Define start moneyness for Aluminium and USDNOK, to use as index
  startAluminium <- round(min(dataIVAluminium[,2])*100)
  startUSDNOK <- round(min(dataIVUSDNOK[,2])*100)
  
  # Convenience yield ----------------------------------------------------------
  
  # Find closest available date in data and save that data
  # If no data available, use the closest earlier date available
  marketPriceFuture <- findClosestAvailableDate(today, allMarketPricesFuture,
                                                "backward")[,2:49]
  
  # Calculate convenience yield
  conYield <- conYieldForward(matchVector(rNOK, tauFuture, D), tauFuture,
                              as.numeric(marketPriceFuture)*pricesUSDNOK[1,1],
                              pricesAluminium[1,1])
  
  

  # Make the convenience yield stochastic
  
  # In 0, all convenience yields are stochastic
  stochasticConYieldMatch0 <- stochasticConYield(conYield[c(1,3,6,12,24,36,48)], n, tau)
  
  # In 1, the 1m convenience yield is not stochastic
  stochasticConYieldMatch1 <- stochasticConYield1m(conYield[c(1,3,6,12,24,36,48)], n, tau)
  
  stochasticConYieldMatch1Inter <- matrix(,nrow=n,ncol=1440)
  for (i in 1:n) {
    stochasticConYieldMatch1Inter[i,] <- interpolateVector(tau, stochasticConYieldMatch1[,i], D)
  }
  stochasticConYieldMatch0Inter <- interpolateVector(tau, stochasticConYieldMatch0, D)
  
  # Combine risk free rate and convenience yield
  tau1 <- tau- 1/12
  rMatch0 <- matchVector(rNOK, tau, D)-stochasticConYieldMatch0
  rMatch1 <- matchVector(rNOK, tau1, D)-stochasticConYieldMatch1
  
  
  # Long forward valuation -----------------------------------------------------
  
  # Valuation of individual risk factors
  priceLongAluminium <- createForwardMatrix("long", pricesAluminium, rMatch0,
                                            rMatch1, tau, tau1)
  priceLongIngot     <- createForwardMatrix("long", pricesIngot, rMatch0,
                                            rMatch1, tau, tau1)
  priceLongAlumina   <- createForwardMatrix("long", pricesAlumina, rMatch0,
                                            rMatch1, tau, tau1)
  priceLongCoke      <- createForwardMatrix("long", pricesCoke, rMatch0,
                                            rMatch1, tau, tau1)
  priceLongOil       <- createForwardMatrix("long", pricesOil, rMatch0,
                                            rMatch1, tau, tau1)
  priceLongCaustic   <- createForwardMatrix("long", pricesCaustic, rMatch0,
                                            rMatch1, tau, tau1)
  priceLongPitch     <- createForwardMatrix("long", pricesPitch, rMatch0,
                                            rMatch1, tau, tau1)
  priceLongCoal      <- createForwardMatrix("long", pricesCoal, rMatch0,
                                            rMatch1, tau, tau1)
  priceLongUSDNOK    <- createForwardMatrix("long", pricesUSDNOK,
                                            matchVector(rNOK-rUSD, tau, D)-stochasticConYieldMatch0,
                                            matchVector(rNOK-rUSD, tau1, D)-stochasticConYieldMatch1,
                                            tau, tau1)
  priceLongBRLNOK    <- createForwardMatrix("long", pricesBRLNOK,
                                            matchVector(rNOK-rBRL, tau, D)-stochasticConYieldMatch0,
                                            matchVector(rNOK-rBRL, tau1, D)-stochasticConYieldMatch1,
                                            tau, tau1)
  priceLongEURNOK    <- createForwardMatrix("long", pricesEURNOK,
                                            matchVector(rNOK-rEUR, tau, D)-stochasticConYieldMatch0,
                                            matchVector(rNOK-rEUR, tau1, D)-stochasticConYieldMatch1,
                                            tau, tau1)
  
  # Bind all matrices into one
  priceLong <- rbind(priceLongAluminium, priceLongIngot, priceLongAlumina,
                     priceLongCoke, priceLongOil, priceLongCaustic,
                     priceLongPitch, priceLongCoal, priceLongUSDNOK,
                     priceLongBRLNOK, priceLongEURNOK)
  
  
  # Short forward valuation ----------------------------------------------------
  
  # Valuation of individual risk factors
  priceShortAluminium <- createForwardMatrix("short", pricesAluminium, rMatch0,
                                             rMatch1, tau, tau1)
  priceShortIngot     <- createForwardMatrix("short", pricesIngot, rMatch0,
                                             rMatch1, tau, tau1)
  priceShortAlumina   <- createForwardMatrix("short", pricesAlumina, rMatch0,
                                             rMatch1, tau, tau1)
  priceShortCoke      <- createForwardMatrix("short", pricesCoke, rMatch0,
                                             rMatch1, tau, tau1)
  priceShortOil       <- createForwardMatrix("short", pricesOil, rMatch0,
                                             rMatch1, tau, tau1)
  priceShortCaustic   <- createForwardMatrix("short", pricesCaustic, rMatch0,
                                             rMatch1, tau, tau1)
  priceShortPitch     <- createForwardMatrix("short", pricesPitch, rMatch0,
                                             rMatch1, tau, tau1)
  priceShortCoal      <- createForwardMatrix("short", pricesCoal, rMatch0,
                                             rMatch1, tau, tau1)
  priceShortUSDNOK    <- createForwardMatrix("short", pricesUSDNOK,
                                             matchVector(rNOK-rUSD, tau, D)-stochasticConYieldMatch0,
                                             matchVector(rNOK-rUSD, tau1, D)-stochasticConYieldMatch1,
                                             tau, tau1)
  priceShortBRLNOK    <- createForwardMatrix("short", pricesBRLNOK,
                                             matchVector(rNOK-rBRL, tau, D)-stochasticConYieldMatch0,
                                             matchVector(rNOK-rBRL, tau1, D)-stochasticConYieldMatch1,
                                             tau, tau1)
  priceShortEURNOK    <- createForwardMatrix("short", pricesEURNOK,
                                             matchVector(rNOK-rEUR, tau, D)-stochasticConYieldMatch0,
                                             matchVector(rNOK-rEUR, tau1, D)-stochasticConYieldMatch1,
                                             tau, tau1)
  
  # Bind all matrices into one
  priceShort <- rbind(priceShortAluminium, priceShortIngot, priceShortAlumina,
                      priceShortCoke, priceShortOil, priceShortCaustic,
                      priceShortPitch, priceShortCoal, priceShortUSDNOK,
                      priceShortBRLNOK, priceShortEURNOK)
  
  
  # Put option valuation -------------------------------------------------------
  
  # Valuation of individual risk factors
  pricePutAluminium <- createOptionMatrix(pricesAluminium[2,],
                                          rep(moneynessAluminium,times=length(tau)),
                                          matrix(rep(t(matchVector(rNOK, tau1, D)-stochasticConYieldMatch1),
                                                     times=length(moneynessAluminium)),ncol=n,byrow=TRUE),
                                          sigmaAluminium, 
                                          rep(tau1,times=length(moneynessAluminium)),
                                          D, startAluminium)
  pricePutUSDNOK <- createOptionMatrix(pricesUSDNOK[2,], moneynessUSDNOK, 
                                       matrix(rep(t(matchVector(rNOK-rUSD, tau1, D)-stochasticConYieldMatch1)
                                                  ,times=length(moneynessUSDNOK)),ncol=n,byrow=TRUE),
                                       sigmaUSDNOK, 
                                       rep(tau1,times=length(moneynessUSDNOK)),
                                       D, startUSDNOK)
  
  # Bind all matrices into one
  pricePut <- rbind(pricePutAluminium, pricePutUSDNOK)
  
  
  # Initial put option valuation -----------------------------------------------
  
  # Valuation of individual risk factors
  price0PutAluminium <- createOptionMatrix(matrix(pricesAluminium[1,1]), rep(moneynessAluminium,times=length(tau)),
                                           matrix(rep(t(matchVector(rNOK, tau, D)-stochasticConYieldMatch0),times=length(moneynessAluminium)),ncol=n,byrow=TRUE), sigmaAluminium, 
                                           rep(tau,times=length(moneynessAluminium)), D, startAluminium)
  price0PutUSDNOK <- createOptionMatrix(matrix(pricesUSDNOK[1,1]), moneynessUSDNOK, 
                                        matrix(rep(t(matchVector(rNOK-rUSD, tau, D)-stochasticConYieldMatch0),times=length(moneynessUSDNOK)),ncol=n,byrow=TRUE), sigmaUSDNOK, 
                                        rep(tau,times=length(moneynessUSDNOK)), D, startUSDNOK)
  
  # Bind all matrices into one
  price0Put <- rbind(price0PutAluminium, price0PutUSDNOK)
  
  # Transaction cost -----------------------------------------------------------
  costForward <- calcCost(commission, spotForward)
  costOption <- calcCost(commission, spotPut)
  
  # Total value of risk factors without hedge ----------------------------------
  
  # Net present value of the exposure of risk factors
  sumExposure <- calcExposure(exposureInstruments, months, rNOK, rBRL, rEUR, rUSD, D)
  
  # Net present value of the exposure of risk factors in 1m
  sumExposure1m <- calcExposure(exposureInstruments, months-1, rNOK, rBRL, rEUR, rUSD, D)
  
  # Calculate total initial value of risk factors
  value0RiskFactors <- calcValueRiskFactors(spot, sumExposure)
  
  # Calculate total value of risk factors for each scenario
  valueRiskFactors <- calcValueRiskFactors(pricesRiskFactors, sumExposure1m)

  # Old contracts --------------------------------------------------------------
  
  # Calculate value of the old contracts in 1m
  H <- numeric(n)
  for (scenario in 1:n) {
    temp <- calcOld(oldContracts, ymd(today) %m+% months(1), D, pricesRiskFactors[,scenario],
                           rNOK-stochasticConYieldMatch1Inter[scenario,], rBRL, rEUR, rUSD, sigmaAluminium, sigmaUSDNOK,
                           startAluminium, startUSDNOK)
    H[scenario] <- temp[[1]] + temp[[2]]
  }
  
  # Calculate value of the old contracts 
  H0andCash <- calcOld(oldContracts, today, D, spot, rNOK-stochasticConYieldMatch0Inter, rBRL, rEUR, rUSD,
                       sigmaAluminium, sigmaUSDNOK, startAluminium, startUSDNOK)
  H0 <- H0andCash[[1]]
  
  # Add cash flow to cash ------------------------------------------------------
  nu0 <- nu0 + H0andCash[[2]] + saveCash/100*sum(exposure*percentExposure/100*spot)
  cash <- cash + saveCash/100*sum(exposure*percentExposure/100*spot)
  
  # Optimization ----------------------------------------------------------------
  
  # If Aluminum and/or USDNOK is included in instruments, use optimization model
  # that includes put options
  # Checks witch objective function to use
  if (exists("startIndexPut")) {
    if (optType == "variance") {
      hedge <- optimization(optType, valueRiskFactors, priceLong[indexForward,],
                            priceShort[indexForward,], kappaForward, kappaPut,
                            costForward, nu0, rNOK[30], H, value0RiskFactors,
                            H0, beta, lambda, workingDirectory, pricePut=pricePut[indexPut,],
                            costOption=costOption, 
                            price0Put=price0Put[indexPut,], solver="cplex", 
                            amplDirectory = amplDir)
      }
    else if (optType == "cvar") {
      hedge <- optimization(optType, valueRiskFactors, priceLong[indexForward,],
                            priceShort[indexForward,], kappaForward, kappaPut,
                            costForward, nu0, rNOK[30], H, value0RiskFactors,
                            H0, beta, lambda, workingDirectory, pricePut=pricePut[indexPut,],
                            costOption = costOption,
                            price0Put=price0Put[indexPut,], solver="cplex",
                            amplDirectory = amplDir)
    }
    else if (optType == "utility") {
      hedge <- optimization(optType, valueRiskFactors, priceLong[indexForward,],
                            priceShort[indexForward,], kappaForward, kappaPut,
                            costForward, nu0, rNOK[30], H, value0RiskFactors,
                            H0, beta, lambda, workingDirectory, pricePut=pricePut[indexPut,],
                            costOption = costOption,
                            price0Put=price0Put[indexPut,], solver="minos",
                            amplDirectory = amplDir)
      }
  }
  else {
    if (optType == "variance") {
      hedge <- optimization(optType, valueRiskFactors, priceLong[indexForward,],
                            priceShort[indexForward,], kappaForward, kappaPut,
                            costForward, nu0, rNOK[30], H, value0RiskFactors,
                            H0, beta, lambda, workingDirectory, solver="cplex",
                            amplDirectory = amplDir)
      }
    else if (optType == "cvar") {
      hedge <- optimization(optType, valueRiskFactors, priceLong[indexForward,],
                            priceShort[indexForward,], kappaForward, kappaPut,
                            costForward, nu0, rNOK[30], H, value0RiskFactors,
                            H0, beta, lambda, workingDirectory, solver="cplex",
                            amplDirectory = amplDir)
      }
    else if (optType == "utility") {
      hedge <- optimization(optType, valueRiskFactors, priceLong[indexForward,],
                            priceShort[indexForward,], kappaForward, kappaPut,
                            costForward, nu0, rNOK[30], H, value0RiskFactors,
                            H0, beta, lambda, workingDirectory, solver="minos",
                            amplDirectory = amplDir)
      }
  }
  
  # Change NA to 0
  hedge[[1]][is.na(hedge[[1]])] <- 0
  
  # Net amount of contracts bought
  for (contract in 1:dim(hedge[[1]])[1]) {
    if (hedge[[1]]$alphaLong.val[contract] <= hedge[[1]]$alphaShort.val[contract]) {
      hedge[[1]]$alphaShort.val[contract] <- hedge[[1]]$alphaShort.val[contract]-hedge[[1]]$alphaLong.val[contract]
      hedge[[1]]$alphaLong.val[contract] <- 0
    }
    else {
      hedge[[1]]$alphaLong.val[contract] <- hedge[[1]]$alphaLong.val[contract]-hedge[[1]]$alphaShort.val[contract]
      hedge[[1]]$alphaShort.val[contract] <- 0
    }
    if (exists("startIndexPut")) {
      if (hedge[[1]]$alphaPut.val[contract] < 10) {
        hedge[[1]]$alphaPut.val[contract] <- 0
      }
    }
    if (hedge[[1]]$alphaLong.val[contract] < 10) {
      hedge[[1]]$alphaLong.val[contract] <- 0
    }
    if (hedge[[1]]$alphaShort.val[contract] < 10) {
      hedge[[1]]$alphaShort.val[contract] <- 0
    }
  }
  
  # Calculate objective value and save bought contracts ------------------------
  if (exists("startIndexPut")) {
    if (optType == "variance") {
      objectiveValue <- calcVariance(valueRiskFactors, H, hedge[[1]],
                                     priceLong[indexForward,],
                                     priceShort[indexForward,], hedge[[2]],
                                     cash, rNOK[30], today, alphaStopForward,
                                     alphaStopPut, pricePut=pricePut[indexPut,])
    }
    else if (optType == "cvar") {
      objectiveValue <- calcCVAR(beta, valueRiskFactors, value0RiskFactors, H,
                                 H0, hedge[[1]], priceLong[indexForward,],
                                 priceShort[indexForward,], hedge[[2]], nu0,
                                 cash, rNOK[30], today, alphaStopForward,
                                 alphaStopPut, pricePut=pricePut[indexPut,])
    }
    else if (optType == "utility") {
      objectiveValue <- calcUtility(beta, lambda, valueRiskFactors,
                                    value0RiskFactors, H, H0, hedge[[1]],
                                    priceLong[indexForward,],
                                    priceShort[indexForward,], hedge[[2]], nu0,
                                    cash, rNOK[30], today, alphaStopForward,
                                    alphaStopPut, pricePut=pricePut[indexPut,])
    }
    newPut <- saveOld(hedge[[1]], tau, 
                      c(rep(moneynessAluminium,times=length(tau)),moneynessUSDNOK),
                      rNOK, rBRL, rEUR, rUSD, today, spot, D, 2,
                      startIndexForward, startIndexPut = startIndexPut)
    newLong <- saveOld(hedge[[1]], tau,
                       c(rep(moneynessAluminium,times=length(tau)),moneynessUSDNOK),
                       rNOK, rBRL, rEUR, rUSD, today, spot, D, 3,
                       startIndexForward, startIndexPut = startIndexPut)
    newShort <- saveOld(hedge[[1]], tau,
                        c(rep(moneynessAluminium,times=length(tau)),moneynessUSDNOK),
                        rNOK, rBRL, rEUR, rUSD, today, spot, D, 4,
                        startIndexForward, startIndexPut = startIndexPut)
  }
  else {
    if (optType == "variance") {
      objectiveValue <- calcVariance(valueRiskFactors, H, hedge[[1]],
                                     priceLong[indexForward,],
                                     priceShort[indexForward,], hedge[[2]],
                                     cash, rNOK[30], today, alphaStopForward,
                                     alphaStopPut)
    }
    else if (optType == "cvar") {
      objectiveValue <- calcCVAR(beta, valueRiskFactors, value0RiskFactors, H,
                                 H0, hedge[[1]], priceLong[indexForward,],
                                 priceShort[indexForward,], hedge[[2]], nu0,
                                 cash, rNOK[30], today, alphaStopForward,
                                 alphaStopPut)
    }
    else if (optType == "utility") {
      objectiveValue <- calcUtility(beta, lambda, valueRiskFactors,
                                    value0RiskFactors, H, H0, hedge[[1]],
                                    priceLong[indexForward,],
                                    priceShort[indexForward,], hedge[[2]], nu0,
                                    cash, rNOK[30], today, alphaStopForward,
                                    alphaStopPut)
    }
    newPut <- oldContracts[[1]]
    newLong <- saveOld(hedge[[1]], tau,
                       c(rep(moneynessAluminium,times=length(tau)),moneynessUSDNOK),
                       rNOK, rBRL, rEUR, rUSD, today, spot, D, 2,
                       startIndexForward)
    newShort <- saveOld(hedge[[1]], tau,
                        c(rep(moneynessAluminium,times=length(tau)),moneynessUSDNOK),
                        rNOK, rBRL, rEUR, rUSD, today, spot, D, 3,
                        startIndexForward)
  }

  # Calculate portfolio value --------------------------------------------------
  portfolioValue <- calcPortfolioValue(calcValueRiskFactors(spot, exposure), H0, today, hedge[[2]],
                                       cash, rNOK[30])
  
  # Save information to return -------------------------------------------------
  oldContracts[[1]] <- rbind(oldContracts[[1]], newPut)
  oldContracts[[2]] <- rbind(oldContracts[[2]], newLong)
  oldContracts[[3]] <- rbind(oldContracts[[3]], newShort)
  oldContracts[[4]] <- rbind(oldContracts[[4]], portfolioValue)
  oldContracts[[5]] <- rbind(oldContracts[[5]], objectiveValue)
  
  return(oldContracts)
}

