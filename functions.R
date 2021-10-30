# ----- Risk-free rate -----

# Calculates the risk-free rate from OIS-rates
# Input:  - timeMat: A 2x7 matrix where there is a time fraction in column 1
#                    and an OIS-rate in column 2
# Return: - Risk-free rates for the dates specified in column 1 of timeMat
calcRiskFree <- function(timeMat) {
  
  rPoints <- numeric(0)
  count <- length(which(timeMat[1,] <= 1))
  
  for(i in 1:count){
    timeFrac <- timeMat[1,i]
    rate <- timeMat[2,i]/100
    rPoints <- c(rPoints, log(1+rate*timeFrac)/timeFrac)}
  
  for(i in (count+1):dim(timeMat)[2]) {
    timeFrac <- timeMat[1,i]
    rate <- timeMat[2,i]/100
    tempSum <- 0
    for(j in count:(i-1)) {
      if(j == count){startTime <- 0} # Time increment is from 0 to 1 for 2 year 
      else{startTime <- timeMat[1,j-1]}
      endTime <- timeMat[1,j]
      tempSum <- tempSum + 
        (endTime-startTime)*exp(-rPoints[j]*(endTime-startTime))}
    
    tempRate <- rate*tempSum
    # if (tempRate > 0.8) {tempRate = 0.8}
    rPoints <- c(rPoints, log((1+rate*timeFrac)/(1-tempRate))/timeFrac)}
  
  return(rPoints)
}


# Calculates the interpolated risk-free rate from OIS-rates
# Input:  - tau:      A vector of time to maturities for the OIS-rates 
#                     given as time fractions
#         - valToday: A vector with the OIS-rates for today
#         - D:        Day-count convention
# Return: - Interpolated risk-free rates
OIStoR <- function(tau, valToday, D) {
  OIS <- matrix(c(tau, valToday), nrow = 2, byrow = TRUE)
  r <- calcRiskFree(OIS)
  r <- interpolateVector(tau, r, D)
  return(r)
}


# Calculates the interpolated risk-free rate from swap-rates (used for BRL)
# Input:  - tauDomesticSwap:    A vector of time to maturity for the swap-rates 
#                               given as time fractions for domestic currency
#         - tauForeignRate:     A vector of time to maturities for the foreign 
#                               risk-free rate given as time fractions
#         - todaySwapData:      Current swap rates
#         - rateForeign:        Current risk free rate of foreign currency
#         - D:                  Day-count convention
# Return: - Interpolated risk-free rates of domestic currency
swapToRate <- function(tauDomesticSwap, tauForeignRate, 
                       todaySwapData, rateForeign, D) {
  commonTau <- intersect(tauDomesticSwap,tauForeignRate)
  rateDomestic <- numeric(length(commonTau))
  spot <- todaySwapData[1]
  for (i in 1:length(commonTau)) {
    rateDomestic[i] = rateForeign[match(commonTau[i],tauForeignRate)] + 
      log(todaySwapData[i+1]/spot) / 
      tauDomesticSwap[match(commonTau[i],tauDomesticSwap)]
  }
  
  return(interpolateVector(commonTau, rateDomestic, D))
}


# Transforms a deterministic conveniece yield into a stochastic one.
# This function calculates as if we are standing one month in the future.
# Input:  - conYieldMatch:  The deterministic yields
#         - nScenario:      Number of scenarios
#         - tau:            Time fractions for the dates with yields
# Return: A vector with stochastic yields
stochasticConYield1m <- function(conYieldMatch, nScenario, tau) {
  stochasticConYieldMatch <- matrix(,nrow=length(conYieldMatch),ncol=nScenario)
  for (date in 1:length(conYieldMatch)) {
    if (date == 1) {
      stochasticConYieldMatch[date,] <- rep(conYieldMatch[date], 
                                            times=nScenario)
    }
    else {
      stochasticConYieldMatch[date,] <- conYieldMatch[date] + rnorm(nScenario)/
        (100*tau[date]) #div. time to mat.
    }
  }
  return(stochasticConYieldMatch)
}


# Transforms a deterministic conveniece yield into a stochastic one.
# This function calculates as if we are standing at the current date.
# Input:  - conYieldMatch:  The deterministic yields
#         - nScenario:      Number of scenarios
#         - tau:            Time fractions for the dates with yields
# Return: A vector with stochastic yields
stochasticConYield <- function(conYieldMatch, nScenario, tau) {
  stochasticConYieldMatch <- matrix(,nrow=length(conYieldMatch),ncol=nScenario)
  for (date in 1:length(conYieldMatch)) {
    stochasticConYieldMatch[date,] <- conYieldMatch[date] + rnorm(nScenario)/
      (100*tau[date])
  }
  return(stochasticConYieldMatch)
}


# ------ Forward -----

# Calculates the strike price of Forward contracts with different maturities
# Input:  - S0:       Price of underlying asset
#         - tau:      A vector of time to maturities given as time fractions
#         - rMatched: The rates corresponding to the maturity dates
# Return: A vector with strike prices for all the maturities
calcK <- function(S0, tau, rMatched) {
  S0 <- S0
  tau <- tau
  rMatched <- rMatched
  K <- S0 * exp(rMatched*tau)
  return(K)
}


# Calculates the value of a single long/short forward contract
# Input:  - S0:  Price of underlying
#         - K:   Strike price of contract
#         - r:   The risk-free rate for the given tau
#         - tau: Time to maturity for contract given as time fraction
# Return: The value of the forward contract
valLongForward <- function(S0, K, r, tau) {
  value <- (S0*exp(r*tau)-K)*exp(-r*tau)
  if (tau < 0) {
    value <- 0
  }
  return(value)
}

valShortForward <- function(S0, K, r, tau) {
  value <- (K-S0*exp(r*tau))*exp(-r*tau) 
  if (tau < 0) {
    value <- 0
  }
  return(value)
}


# Creates a matrix with the values of the forward contracts in each scenario
# Input:  - type:   long/short
#         - prices: spot + scenario prices, from Erik and Victor
#         - rMatch: Vector of matched risk-free rates
#         - tau:    Vector with the time to maturities for the contracts in
#                   the optimization given as time fractions
# Return: Matrix of size [length(tau), number of scenarios]
# Will be run 2*11 times
createForwardMatrix <- function(type, prices, rMatch0, rMatch1, tau0, tau1) {
  Nscenarios <- dim(prices)[2]
  Ndates <- length(tau0)
  K <- calcK(prices[1,1], tau0, rMatch0)
  forwardMatrix <- matrix(,nrow = Ndates, ncol = Nscenarios)
  for (scenario in 1:Nscenarios) {
    for (date in 1:(Ndates)) {
      if (type == "long") {
        forwardMatrix[date,scenario] <- valLongForward(prices[2,scenario], 
                                                       K[date, scenario], 
                                                       rMatch1[date,scenario], 
                                                       tau1[date])
      } else if (type == "short") {
        forwardMatrix[date,scenario] <- valShortForward(prices[2,scenario], 
                                                        K[date, scenario], 
                                                        rMatch1[date,scenario], 
                                                        tau1[date])
      } else {
        print("Wrong type")
      }
    }
  } 
  return(forwardMatrix)
}


# ----- Put -----

# Transform the delta of an option to equivalent strike price (Put)
# Input:  - delta: Delta value for a contract
#         - sigma: Sigma value for a contract
#         - tau:   Time to maturity for a contract given as time fraction
# Return: Moneyness (strike/spot)
deltaToStrikeForPut <- function(delta, sigma, tau) {
  return(exp((sigma/200)*tau-
               qnorm(1-as.numeric(delta)/100)*sigma/100*sqrt(tau)))
}


# Calculates D1 and D2 for Black-Scholes
# Input:  - moneyness: strike price of option/Spot price of the underlying asset
#         - rMatch:    Matched risk-free rate for maturity date
#         - sigma:     Volatility for maturity date
#         - tau:       Time to maturity for option given as time fraction
# Return: D1 and D2 value
calcD1 <- function(moneyness, rMatch, sigma, tau) {
  sigma <- sigma/100
  numerator <- log(1/moneyness) + (rMatch + sigma^2/2)*tau
  denominator <- sigma*sqrt(tau)
  return(numerator/denominator)
}

calcD2 <- function(moneyness, rMatch, sigma, tau) {
  sigma <- sigma/100
  numerator <- log(1/moneyness) + (rMatch - sigma^2/2)*tau
  denominator <- sigma*sqrt(tau)
  return(numerator/denominator)
}


# Calculates the value of a put option according to Black-Scholes
# Input:  - S0:        Spot price of the underlying asset
#         - moneyness: Strike price of the option/S0
#         - rMatch:    Matched risk-free rate for maturity date
#         - sigma:     Volatility for maturity date
#         - tau:       Time to maturity for an option given as time fraction
# Return: The value of a put option
valPut <- function(S0, moneyness, rMatch, sigma, tau) {
  d1 <- calcD1(moneyness, rMatch, sigma, tau)
  d2 <- calcD2(moneyness, rMatch, sigma, tau)
  value <- moneyness*S0*exp(-rMatch*tau)*pnorm(-d2)-S0*pnorm(-d1)
  if (tau == 0) {
    value <- max(0,S0*(moneyness-1))
  }
  else if (tau < 0) {
    value <- 0
  }
  return(value)
}


# Creates a matrix with the values of the put contracts in each scenario
# Input:  - prices:    spot + scenario prices, from Erik and Victor
#         - moneyness: Strike/spot
#         - rMatch:    Vector of matched risk-free rates
#         - sigma:     Volatility for maturity date
#         - tau:       Vector with the time to maturities for the contracts in
#                      the optimization given as time fractions
#         - D:         Day-count convention
# Return: Matrix of size [length(tau), number of scenarios]
# Will be run 2*2 times
createOptionMatrix <- function(prices, moneyness, rMatch, 
                               sigma, tau, D, start) {
  Nscenarios <- length(prices)
  Ndates <- length(tau)
  optionMatrix <- matrix(,nrow = length(moneyness), ncol = Nscenarios)
  for (scenario in 1:Nscenarios) {
    price <- prices[1,scenario]
    for (strike in 1:length(moneyness)) {
      optionMatrix[strike,scenario] <- 
        valPut(price, moneyness[strike], 
               rMatch[strike,scenario], 
               sigma[(round(moneyness[strike]*100)-start + 1),
                     (round(tau[strike]*D))], 
               tau[strike])
    }
  } 
  return(optionMatrix)
}


# ----- Find data -----

# Finds the first date that contains no NaN values, starting
# with today. It looks forward or backwards in time from today.
# Input:  - today: Today's date as a date-object
#         - data: A dataframe containing all data
#         - direction: Is "forward" or "backward" for direction in time
# Return: The date closest to today that contains no NaN vaues (date-object)
findClosestAvailableDate <- function(today, data, direction) {
  stayInLoop <- TRUE
  if (direction == "backward") {
    while(stayInLoop) {
      today <- ymd(today) %m-% days(1)
      stayInLoop <- !(!is.na(match(as_datetime(today),data$Date)) &
                        !anyNA(data[match(as_datetime(today),data$Date),]))
    }
  } else if (direction == "forward") {
    while(stayInLoop) {
      today <- ymd(today) %m+% days(1)
      stayInLoop <- !(!is.na(match(as_datetime(today),data$Date)) &
                        !anyNA(data[match(as_datetime(today),data$Date),]))
    }
  }
  return(data[match(as_datetime(today),data$Date),])
}


# Returns the values of a vector for the dates specified in tau
# Input:  - vector: A vector with with one value for each day
#         - tau:    The time to maturities given as time fractions
#         - D:      The day-count convention (usually 360 or 365)
# Return: The values matching the dates, vector of length(tau)
matchVector <- function(vector, tau, D) {
  Match <- numeric(length(tau))
  for (i in 1:length(tau)) {
    if (tau[i] > 0) {
      Match[i] <- vector[round(tau[i]*D)]
    }
    else {
      Match[i] <- 0
    }
  }
  return(Match)
}


# Returns the values of a matrix for the dates specified in tau
# Input:  - matrixInput: A matrix with n values for each day
#         - tau:         The time to maturities given as time fractions
# Return: The values matching the dates, matrix of size n x length(tau)
matchMatrix <- function(matrixInput, tau) {
  Match <- matrix(,length(tau),dim(matrixInput)[2])
  for (i in 1:length(tau)) {
    if (tau[i] > 0) {
      Match[i,] <- matrixInput[tau[i]*12,]
    }
    else {
      Match[i,] <- 0
    }
  }
  return(Match)
}


# Returns one volatility from the sigma matrixfor the date 
# and strike specified in tau and moneyness
# Input:  - moneyness: Strike/spot
#         - tau:       The time to maturities given as time fractions
#         - D:         The day-count convention (usually 360 or 365)
#         - sigma:     The matrix of volatilites [moneyness, date]
#         - start:     The moneyness value that has index 1 in sigma
# Return: The volatility for tau and moneyness
matchSigma <- function(moneyness, tau, D, sigma, start) {
  m <- round(moneyness*100)
  time <- round(tau*D)
  if (m <= start) {
    Match <- sigma[1,time]
  }
  else if (m-start > dim(sigma)[1]) {
    Match <- sigma[dim(sigma)[1],time]
  }
  else {
    Match <- sigma[m-start, time]
  }
}


# ----- Interpolation -----

# Creates a vector with daily values from "points" by interpolation
# Input:  - tau:    Vector of time to maturities for the "points" (time fracs) 
#         - Points: Known values for specific time to maturities
#         - D:      Day-count convention
# Return: A vector with daily values
interpolateVector <- function(tau, Points, D) {
  end <- length(tau)
  vec <- numeric(round(tau[end]*D))
  date <- round(tau[1]*D)
  for(t in 1:date){
    vec[t] <- Points[1]
  }
  
  for(i in 2:end){
    startDate <- round(tau[i-1]*D)
    endDate <- round(tau[i]*D)
    for(t in (startDate+1):(endDate)){
      vec[t] <- Points[i-1] + 
        (Points[i]-Points[i-1])*(t-startDate)/(endDate-startDate)
    }
  }
  
  return(vec)
}


# Creates a vector with interpolated values
# Input:  - vec: Vector where the first and last values are known, others are NA
# Return: A vector with the former NA values, length(vec)-2
interpolate <- function(vec) {
  endVal <- vec[length(vec)]
  for (i in 2:length(vec)-1) {
    vec[i] <- vec[1] + (endVal - vec[1])/(length(vec)-1) * (i - 1)
  }
  return(vec[2:(length(vec)-1)])
}


# Creates a matrix with daily values from "points" by interpolation
# Input:  - tau: Vector of time to maturities for the "points", (as time fracs)
#         - D:   Day-count convention
#         - vol: A matrix of size [contracts, 3], first column contain the time
#                to maturity (time fractions), the second column the moneyness 
#                and the third column the IV for each contract
# Return: A matrix with IV for each day and moneyness
interpolateMatrix <- function(tau, D, vol) {
  times <- round(tau*D)
  #vol <- as.numeric(vol)
  #vol <- matrix(vol, ncol=3)
  start <- round(min(vol[,2])*100)
  rows <- round(max(vol[,2])*100)-start+1
  cols <- max(tau)*D
  if (cols < 4*D) {
    volMat <- matrix(, nrow = rows, ncol = 1440)
  }
  else {
    volMat <- matrix(, nrow = rows, ncol = cols)
  }
  for (index in 1:dim(vol)[1]) {
    t <- round(as.numeric(vol[index,1])*D)
    moneyness <- round(as.numeric(vol[index,2])*100)- start+1
    volMat[moneyness, t] <- as.numeric(vol[index, 3])
  }
  for (i in 1:length(tau)) {
    t <- times[i]
    index1 <- 1
    for (j in 1:rows) {
      if (!is.na(volMat[j,t])) {
        index2 <- j
        if (is.na(volMat[1,t])) {
          volMat[1:j, t] <- rep(c(volMat[j,t]),times=j)
        }
        else if (index1 == index2 || index2 == (index1+1)) {}
        else {
          volMat[(index1+1):(index2-1), t] <- interpolate(volMat[index1:index2, 
                                                                 t])
        }
        index1 <- j
      }
      else if (j == rows) {
        if (is.na(volMat[j,t])) {
          volMat[(index1+1):j, t] <- rep(c(volMat[index1,t]),times=(j-index1))
        }
      }
    }
  }
  for (t in 1:times[1]-1) {
    volMat[,t] <- volMat[,times[1]]
  }
  for (t in 2:length(times)) {
    for (j in 1:rows) {
      volMat[j,(times[t-1]+1):(times[t]-1)] <- 
        interpolate(volMat[j,(times[t-1]):(times[t])])
    }
  }
  if (is.na(volMat[1,1440])) {
    for (i in (cols+1):1440) {
      volMat[, i] <- volMat[,cols]
    }
  }
  return(volMat)
}


# ----- Old contracts -----

# Saves the info of the "bought" contracts into a dataframe
# Input:  - alpha:               Vector with the amount bought of each contract
#         - tau:                 Vector with time to maturitiy as time fractions
#         - moneyness:           Vector with (strike/spot) for each contract
#         - rNOK/rBRL/rEUR/rUSD: Risk-free rates for each currency
#         - today:               Today's date as a number
#         - S0:                  Today's spot for each underlying asset
#         - D:                   Day-count convention
#         - type:                2=put, 3=forward long, 4=forward short
# Return: Dataframe with amount, strike, time to maturity, 
#         start date and contract numer
#         for each contract
saveOld <- function(alpha, tau, moneyness, rNOK, rBRL, rEUR, rUSD, today, S0, D, 
                    type, startIndexForward, startIndexPut=NULL) {
  for (contract in 1:length(startIndexForward)) {
    startIndexForward[contract] <- startIndexForward[contract] - 7*(contract-1)
  }
  nonZero <- colSums(alpha >= 1)
  amount <- numeric(nonZero[type])
  strike <- numeric(nonZero[type])
  timeToMat <- numeric(nonZero[type])
  startDate <- numeric(nonZero[type])
  whichContract <- numeric(nonZero[type])
  counter <- 1
  for (contract in 1:dim(alpha)[1]) {
    if (alpha[contract,type] >= 1) { # Removes alpha of size < 1
      if (type == 2 & !is.null(startIndexPut)) {
        contractStart <- contract + startIndexPut - 1
      }
      else {
        contractStart <- contract + startIndexForward[ceiling(contract/7)] - 1
      }
      amount[counter] <- alpha[contract,type]
      timeToMat[counter] <- tau[((contractStart-1)%%7)+1]
      timeD <- round(timeToMat[counter]*D)
      startDate[counter] <- today
      whichContract[counter] <- contractStart
      if (type == 2) {
        if (contractStart <= 63) {
          spot <- S0[1]
          r <- rNOK[timeD]
        }
        else {
          spot <- S0[9]
          r <- rNOK[timeD] - rUSD[timeD]
        }
        strike[counter] <- moneyness[contractStart]*spot
      }
      else {
        #if (is.element(contract,43:49)) {
         # r <- rEUR[timeD]
        #}
        #else 
        if (is.element(contractStart,57:63)) {
          r <- rNOK[timeD] - rUSD[timeD]
        }
        else if (is.element(contractStart,64:70)) {
          r <- rNOK[timeD] - rBRL[timeD]
        }
        else if (is.element(contractStart,71:77)) {
          r <- rNOK[timeD] - rEUR[timeD]
        }
        else {
          r <- rNOK[timeD]
        }
        strike[counter] <- calcK(S0[ceiling(contractStart/7)], 
                                 tau[((contractStart-1)%%7)+1], r)
      }
      counter <- counter + 1
    }
  }
  df <- data.frame(amount, strike, timeToMat, startDate, whichContract)
  return(df)
}


# Calculates the value of already obtained contracts for a scenario
# Input:  - oldContracts:               Dataframe with info from old contracts
#         - today:                      Today's date, as a number
#         - D:                          Day-count convention
#         - scenarioPrices:             Scenario price for each risk factor
#         - rNOK/rBRL/rEUR/rUSD:        Risk-free rate for each currency
#         - sigmaAluminium/sigmaUSDNOK: Volatility matrices for each risk factor
#         - startAluminium/startUSDNOK: Moneyness value with index 1 in sigma 
# Return: A list with 2 elements: The value of the old contracts and the amount 
#         of money from recently expired contracts
calcOld <- function(oldContracts, today, D, scenarioPrices, 
                    rNOK, rBRL, rEUR, rUSD, sigmaAluminium, 
                    sigmaUSDNOK, startAluminium, startUSDNOK) {
  H <- 0
  cashFromContracts <- 0
  for (type in 1:3) {
    df <- oldContracts[[type]]
    if (dim(df)[1] > 0) {
      for (contract in 1:dim(df)[1]) {
        time <- (as.numeric(df$timeToMat[contract]) - 
          (as.numeric(today) - as.numeric(as.Date(df$startDate[contract])))/D)
        timeD <- round(time*D)
        if (time > 1/24) {
          if (type == 1) {
            if (df$whichContract[contract] <= 63) {
              scenarioPrice <- scenarioPrices[1]
              r <- rNOK[timeD]
              index <- (round(df$strike[contract]/scenarioPrice*100)-
                          startAluminium+1)
              # If moneyness not in sigma matrix, set to bound
              if (index < 1) {
                index <- 1
              }
              else if (index > dim(sigmaAluminium)[1]) {
                index <- dim(sigmaAluminium)[1]
              }
              sigma <- sigmaAluminium[index, timeD]
            }
            else {
              scenarioPrice <- scenarioPrices[9]
              r <- rNOK[timeD] - rUSD[timeD]
              index <- (round(df$strike[contract]/scenarioPrice*100)-
                          startUSDNOK+1)
              # If moneyness not in sigma matrix, set to bound
              if (index < 1) {
                index <- 1
              }
              else if (index > dim(sigmaUSDNOK)[1]) {
                index <- dim(sigmaUSDNOK)[1]
              }
              sigma <- sigmaUSDNOK[index, timeD]
            }
            price <- valPut(scenarioPrice, df$strike[contract]/scenarioPrice, 
                            r, sigma, time)
            H <- H + price * df$amount[contract]
          }
          else {
            scenarioPrice <- 
              scenarioPrices[ceiling(df$whichContract[contract]/7)]
            if (is.element(df$whichContract[contract],57:63)) {
              r <- rNOK[timeD] - rUSD[timeD]
            }
            else if (is.element(df$whichContract[contract],64:70)) {
              r <- rNOK[timeD] - rBRL[timeD]
            }
            else if (is.element(df$whichContract[contract],71:77)) {
              r <- rNOK[timeD] - rEUR[timeD]
            }
            else {
              r <- rNOK[timeD]
            }
            K <- df$strike[contract]#/scenarioPrice
            if (type == 2) {
              price <- valLongForward(scenarioPrice, K, r, time)
            }
            else {
              price <- valShortForward(scenarioPrice, K, r, time)
            }
            H <- H + price * df$amount[contract]
          }
        } else if (time <= 1/24 && time > -1/24) { # Expired in last 2 weeks
          if (type == 1) {
            if (df$whichContract[contract] <= 63) 
              {scenarioPrice <- scenarioPrices[1]}
            else 
              {scenarioPrice <- scenarioPrices[9]}
            valueAtExpiry <- max(0, df$strike[contract] - scenarioPrice)
          }
          if (type == 2) {
            scenarioPrice <- 
              scenarioPrices[ceiling(df$whichContract[contract]/7)]
            valueAtExpiry <- 
              scenarioPrice - df$strike[contract]
          }
          if (type == 3) {
            scenarioPrice <- 
              scenarioPrices[ceiling(df$whichContract[contract]/7)]
            valueAtExpiry <- 
              df$strike[contract] - scenarioPrice
          }
          cashFromContracts <- cashFromContracts + 
            valueAtExpiry * df$amount[contract]
        }
      }
    }
  }
  return(list(H,cashFromContracts))
}


# ----- Evaluation -----

# Calculates the value of both the unhedged and the hedged portfolio
# Input:  - V:      Value of risk factors
#         - H:      Value of old contracts
#         - today:  Today's date
#         - nu:     Cash left after hedge
#         - cash:   Amount of cash if no hedge took place
#         - r:      Risk-free rate for 1 month
# Return: A dataframe with hedge/unhedge portfolio value, today's date and cash
calcPortfolioValue <- function(V, H, today, nu, cash, r) {
  objHedged <- V + H + nu[[1]][1]/(exp(r/12))
  objUnhedged <- V + cash
  df <- data.frame(objHedged, objUnhedged, today, nu[[1]][1])
  return(df)
}

# Calculates the value of both the unhedged and the hedged portfolio
# Input:  - V:      Value of risk factors
#         - H:      Value of old contracts
#         - today:  Today's date
#         - nu:     Cash left after hedge
#         - cash:   Amount of cash if no hedge took place
#         - r:      Risk-free rate for 1 month
# Return: A dataframe with hedge/unhedge portfolio value, today's date and cash
calcPortfolioValueTransactionCost <- function(V, H, today, nu, cash, r, alpha, alphaStopForward, 
                                              alphaStopPut, costForward, costOption) {
  objHedged <- V + H + nu[[1]][1]/(exp(r/12))
  objUnhedged <- V + cash
  if (alphaStopPut != 0) {
    transactionCost <- 
      sum(costOption*alpha$alphaPut.val[1:alphaStopPut]) + 
      sum(costForward*(alpha$alphaLong.val[1:alphaStopForward]+
                         alpha$alphaShort.val[1:alphaStopForward]))
  }
  else {
    transactionCost <- 
      sum(costForward*(alpha$alphaLong.val[1:alphaStopForward] +
                         alpha$alphaShort.val[1:alphaStopForward]))
  }
  hedged <- V + H + nu[[1]][1]/(exp(r/12)) + transactionCost
  df <- data.frame(objHedged, objUnhedged, today, nu[[1]][1], hedged)
  return(df)
}


# Calculates the variance of the hedge and unhedged portfolio
# Input:  - Vi:                   Value of risk factors in each scenario 
#         - Hi:                   Value of old contracts in each scenario 
#         - alpha:                Number of contracts owned
#         - priceLong/Short:      Price of long and short forward contracts
#         - nu:                   Amount of cash left after hedge
#         - cash:                 Amount of cash if no hedge took place
#         - r:                    Risk-free rate for 1 month
#         - today:                Today's date
#         - alphaStopForward/Put: Index to keep track of where to stop
#         - kappaForward/Put:     Model parameter for optimization model
#         - costForward/Option:   Transaction cost of contracts
#         - pricePut:             Price of put-options (default NULL)
# Return: A dataframe with variance of hedged/unhedged portfolio together 
#         with today's date and the current cash from not hedging
calcVariance <- function(Vi, Hi, alpha, priceLong, priceShort, nu, cash, r, 
                         today, alphaStopForward, alphaStopPut, pricePut=NULL) {
  if (!is.null(pricePut)) {
    hedged <- Vi + Hi + 
      t(t(alpha$alphaPut.val[1:alphaStopPut])%*%pricePut) + 
      t(t(alpha$alphaLong.val[1:alphaStopForward])%*%priceLong) + 
      t(t(alpha$alphaShort.val[1:alphaStopForward])%*%priceShort) + nu$nu.val[1]
  }
  else {
    hedged <- Vi + Hi + 
      t(t(alpha$alphaLong.val[1:alphaStopForward])%*%priceLong) + 
      t(t(alpha$alphaShort.val[1:alphaStopForward])%*%priceShort) + nu$nu.val[1]
  }
  unhedged <- Vi + cash*exp(r/12)
  n <- length(Vi)
  meanHedged <- sum(hedged)/n
  objHedged <- sum((hedged-meanHedged)^2)/(n-1)
  meanUnhedged <- sum(unhedged)/n
  objUnhedged <- sum((unhedged - meanUnhedged)^2)/(n-1)
  df <- data.frame(objHedged, objUnhedged, today, cash*exp(r/12))
  return(df)
}

calcVarianceTransactionCost <- function(Vi, Hi, alpha, priceLong, priceShort, 
                                        nu, cash, r, today, alphaStopForward, 
                                        alphaStopPut, kappaForward, kappaPut, 
                                        costForward, costOption, pricePut=NULL){
  if (!is.null(pricePut)) {
    hedged <- Vi + Hi + t(t(alpha$alphaPut.val[1:alphaStopPut])%*%pricePut) + 
      t(t(alpha$alphaLong.val[1:alphaStopForward])%*%priceLong) + 
      t(t(alpha$alphaShort.val[1:alphaStopForward])%*%priceShort) + nu$nu.val[1]
    transactionCost <- 
      sum(kappaPut*costOption*alpha$alphaPut.val[1:alphaStopPut]) + 
      sum(kappaForward*costForward*(alpha$alphaLong.val[1:alphaStopForward]+
                                      alpha$alphaShort.val[1:alphaStopForward]))
  }
  else {
    hedged <- Vi + Hi + 
      t(t(alpha$alphaLong.val[1:alphaStopForward])%*%priceLong) + 
      t(t(alpha$alphaShort.val[1:alphaStopForward])%*%priceShort) + nu$nu.val[1]
    transactionCost <- 
      sum(kappaForward*costForward*(alpha$alphaLong.val[1:alphaStopForward] +
                                      alpha$alphaShort.val[1:alphaStopForward]))
  }
  unhedged <- Vi + cash*exp(r/12)
  n <- length(Vi)
  meanHedged <- sum(hedged)/n
  objHedged <- sum((hedged-meanHedged)^2)/(n-1) + transactionCost
  meanUnhedged <- sum(unhedged)/n
  objUnhedged <- sum((unhedged - meanUnhedged)^2)/(n-1)
  df <- data.frame(objHedged, objUnhedged, today, cash*exp(r/12))
  return(df)
}


# Calculates the expected shortfall of the hedge and unhedged portfolio
# Input:  - beta:                 Level of VaR (between 0%-100%)
#         - Vi:                   Value of risk factors in each scenario
#         - V0:                   Value of risk factors today (one value)
#         - Hi:                   Value of old contracts in each scenario
#         - Hi:                   Value of old contracts today (one value)
#         - alpha:                Number of contracts owned
#         - priceLong/Short:      Price of long and short forward contracts
#         - nu:                   Amount of cash left after hedge
#         - cash:                 Amount of cash if no hedge took place
#         - r:                    Risk-free rate for 1 month
#         - today:                Today's date
#         - alphaStopForward/Put: Index to keep track of where to stop
#         - kappaForward/Put:     Model parameter for optimization model
#         - costForward/Option:   Transaction cost of contracts
#         - pricePut:             Price of put-options (default NULL)
# Return: A dataframe with the expected shortfall of hedged/unhedged portfolio 
#         together with today's date and the current cash from not hedging
calcCVAR <- function(beta, Vi, V0, Hi, H0, alpha, priceLong, priceShort, nu, 
                     nu0, cash, r, today, alphaStopForward, 
                     alphaStopPut, pricePut=NULL) {
  if (!is.null(pricePut)) {
    hedged <- V0 + H0 + nu0 - 
      (Vi + Hi + t(t(alpha$alphaPut.val[1:alphaStopPut])%*%pricePut) + 
         t(t(alpha$alphaLong.val[1:alphaStopForward])%*%priceLong) + 
         t(t(alpha$alphaShort.val[1:alphaStopForward])%*%priceShort) + 
         nu$nu.val[1])
  }
  else {
    hedged <- V0 + H0 + nu0 - 
      (Vi + Hi + t(t(alpha$alphaLong.val[1:alphaStopForward])%*%priceLong) + 
         t(t(alpha$alphaShort.val[1:alphaStopForward])%*%priceShort) + 
         nu$nu.val[1])
  }
  unhedged <- V0 + cash - (Vi + cash*exp(r/12))
  n <- length(Vi)
  nBeta <- n * beta
  #zeta <- sort(hedged)[nBeta]
  # print(sort(hedged)[nBeta])
  # print(sort(hedged)[nBeta+1])
  yHedged <- sort(hedged)[(nBeta+1):n]
  #notZero <- sum(yHedged != 0)
  # print(sum(yHedged != 0))
  yUnhedged <- sort(unhedged)[(nBeta+1):n]
  objHedged <- sum(yHedged)/(n-nBeta)
  objUnhedged <- sum(yUnhedged)/(n-nBeta)
  df <- data.frame(objHedged, objUnhedged, today, cash*exp(r/12))
  return(df)
}


# Calculates the utility value of the hedge and unhedged portfolio
# Input:  - beta:                 Level of VaR (between 0%-100%)
#         - lamda:                Model parameter for risk aversion (0%-100%)
#         - Vi:                   Value of risk factors in each scenario
#         - V0:                   Value of risk factors today (one value)
#         - Hi:                   Value of old contracts in each scenario
#         - Hi:                   Value of old contracts today (one value)
#         - alpha:                Number of contracts owned
#         - priceLong/Short:      Price of long and short forward contracts
#         - nu:                   Amount of cash left after hedge
#         - cash:                 Amount of cash if no hedge took place
#         - r:                    Risk-free rate for 1 month
#         - today:                Today's date
#         - alphaStopForward/Put: Index to keep track of where to stop
#         - kappaForward/Put:     Model parameter for optimization model
#         - costForward/Option:   Transaction cost of contracts
#         - pricePut:             Price of put-options (default NULL)
# Return: A dataframe with the expected shortfall of hedged/unhedged portfolio 
#         together with today's date and the current cash from not hedging
calcUtility <- function(beta, lambda, Vi, V0, Hi, H0, alpha, priceLong, 
                        priceShort, nu, nu0, cash, r, today, alphaStopForward, 
                        alphaStopPut, pricePut=NULL) {
  if (!is.null(pricePut)){
    hedged <- V0 + H0 + nu0 - 
      (Vi+ Hi + t(t(alpha$alphaPut.val[1:alphaStopPut])%*%pricePut) + 
         t(t(alpha$alphaLong.val[1:alphaStopForward])%*%priceLong) + 
         t(t(alpha$alphaShort.val[1:alphaStopForward])%*%priceShort) + 
         nu$nu.val[1])
  }
  else {
    hedged <- V0 + H0 + nu0 - 
      (Vi + Hi + t(t(alpha$alphaLong.val[1:alphaStopForward])%*%priceLong) + 
         t(t(alpha$alphaShort.val[1:alphaStopForward])%*%priceShort) + 
         nu$nu.val[1])
  }
  unhedged <- V0 + cash - (Vi + cash*exp(r/12))
  n <- length(Vi)
  nBeta <- n * beta
  # print(sort(hedged)[nBeta])
  # print(sort(hedged)[nBeta+1])
  yHedged <- sort(hedged)[(nBeta+1):n]
  yUnhedged <- sort(unhedged)[(nBeta+1):n]
  # print(sum(yHedged != 0))
  if (lambda == 1) {
    objHedged <- lambda*(sum(yHedged)/(n-nBeta))
    objUnhedged <- lambda*(sum(yUnhedged)/(n-nBeta))
  }
  else {
    objHedged <- lambda*(sum(yHedged)/(n-nBeta)) - (1-lambda)*sum(log(-hedged+(V0 + H0 + nu0)))/n
    objUnhedged <- lambda*(sum(yUnhedged)/(n-nBeta)) - (1-lambda)*sum(log(-unhedged+(V0+cash)))/n
  }
  df <- data.frame(objHedged, objUnhedged, today, cash*exp(r/12))
  return(df)
}

# TODO: Dubbelkolla
statisticalTest <- function(df,gamma) {
  hedged <- df[,1]
  unhedged <- df[,2]
  M <- length(hedged)
  d <- numeric(M)
  for (i in 1:M) {
    d[i] <- unhedged[i] - hedged[i]
  }

  dBar <- sum(d) / M

  s <- sqrt( sum((d - dBar)^2) / (M - 1) )

  z <- ( dBar * sqrt(M) / s)
  print(z)

  upperCI <- qnorm(1-gamma/2)
  lowerCI <- -upperCI
  if (lowerCI <= z & z <= upperCI) {
    inCI <- "z in CI"
  }
  else {
    inCI <- "z not in CI"
  }
  toReturn <- data.frame(z, lowerCI, upperCI, inCI)
  print(inCI)
  return(toReturn)
}


# ----- Other functions -----

# Calculates the total exposure to all risk factors including present
# value of future cash flows
# Input:  - exposure:     Exposure volume per risk factor (vector)
#         - numberMonths: Number of months to calculate present value of
#         - rNOK:         Risk-free rate of NOK
#         - rBRL:         Risk-free rate of BRL
#         - rEUR:         Risk-free rate of EUR
#         - rUSD:         Risk-free rate of USD
#         - D:            Day-count convention
# Return: The total exposure
calcExposure <- function(exposure, numberMonths, rNOK, rBRL, rEUR, rUSD, D) {
  months <- c(0:numberMonths)
  rMatch <- matchVector(rNOK, months, D/12)
  sumExposure <- numeric(length(exposure))
  for (riskFactor in 1:length(exposure)) {
    if (riskFactor==9) {
      rMatch <- matchVector(rNOK-rUSD, months, D/12)
    }
    else if (riskFactor==10) {
      rMatch <- matchVector(rNOK-rBRL, months, D/12)
    }
    else if (riskFactor==11) {
      rMatch <- matchVector(rNOK-rEUR, months, D/12)
    }
    sumExposure[riskFactor] <- sum(exposure[riskFactor]*exp(-rMatch*months/12))
  }
  return(sumExposure)
}


# Calculates the value of the risk factors in each scenario
# Input:  - prices:   Price per underlying asset's risk factor in each scenario,  
#                     data frame with n columns and a row per risk factor
#         - exposure: Exposure volume per risk factor, a vector
# Return: The sum of value for all risk factors in each scenario
calcValueRiskFactors <- function(prices, exposure) {
  if (length(prices) == 11) {toReturn <- sum(exposure * prices)}
  else {toReturn <- colSums(exposure * prices)}
  return(toReturn)
}


# Calculates spreads from bid and ask prices
# Input:  - askbBidMatrix: Matrix with time to maturity, ask and bid prices
# Return: Matrix with 2 columns, time to maturity and spread
calcSpreads <- function(askBidMatrix) {
  rows = dim(askBidMatrix)[1]
  spreads <- matrix(nrow = rows, ncol = 2)
  for (i in 1:rows) {
    spreads[i,1] <- askBidMatrix[i,1]
    spreads[i,2] <- askBidMatrix[i,2] - askBidMatrix[i,3] # Ask - Bid
  }
  return(spreads)
}


# Calculates the transaction costs
# Input:  - commission: The percentage commission
#         - prices:     Spot price of underlying asset per risk factor
# Return: The transaction cost per risk factor
calcCost <- function(commission, prices) {
  return(commission*prices)
}


# Structures the data for implied volatility
# Input:  - moneyness: Strike/spot for each contract
#         - valToday:  IV for each contract, for today
#         - tau:       Time to maturity for each contract
# Return: nContracts x 3 matrix, with the columns time to mat, moneyness, IV-val
matrixIV <- function(moneyness, valToday, tau) {
  dataIV = matrix(, nrow=dim(valToday)[2], ncol=3)
  for (k in 1:length(moneyness)) {
    for (t in 1:length(tau)) {
      dataIV[(k-1)*length(tau)+t,1] <- as.numeric(tau[t])
      dataIV[(k-1)*length(tau)+t,2] <- as.numeric(moneyness[k])
      dataIV[(k-1)*length(tau)+t,3] <- as.numeric(valToday[1,(k-1)*
                                                             length(tau)+t])
    }
  }
  return(dataIV)
}


# Calculated the convenience yield from forward-contracts (futures)
# Input:  - rMatch: The risk free rates for the time to maturities, tau
#         - tau:    The time to maturities for the contracts
#         - Fmkt:   The market prices for the contracts
#         - S0:     The spot prices for the underlying assets
# Return: The convenience yield implied by market prices
conYieldForward <- function(r, tau, Fmkt, S0) {
  y <- (r*tau - log(Fmkt/S0))/tau
  return(y)
}


# Renames the contracts from their index to a name that describes them
# Input:  - type: The type of instrument ("option" or "forward")
#         - indexes: A vector of indexes of the instruments to be renamed
# Return: A vector with names that describes the instruments
indexToName <- function(type, indexes) {
  allNames <- numeric(0)
  for (index in indexes) {
  
    if (type == "option") { 
      if (index <= 63) {
        nameRiskfactor <- "Aluminium"
        if (((index) %% 7) == 1) {timeRiskfactor <- "1M"}
        if (((index) %% 7) == 2) {timeRiskfactor <- "3M"}
        if (((index) %% 7) == 3) {timeRiskfactor <- "6M"}
        if (((index) %% 7) == 4) {timeRiskfactor <- "1Y"}
        if (((index) %% 7) == 5) {timeRiskfactor <- "2Y"}
        if (((index) %% 7) == 6) {timeRiskfactor <- "3Y"}
        if (((index) %% 7) == 7) {timeRiskfactor <- "4Y"}
      }
      if (index > 63) {
        nameRiskfactor <- "USDNOK"
        if (((index) %% 7) == 1) {timeRiskfactor <- "1M"}
        if (((index) %% 7) == 2) {timeRiskfactor <- "3M"}
        if (((index) %% 7) == 3) {timeRiskfactor <- "6M"}
        if (((index) %% 7) == 4) {timeRiskfactor <- "1Y"}
        if (((index) %% 7) == 5) {timeRiskfactor <- "2Y"}
        if (((index) %% 7) == 6) {timeRiskfactor <- "3Y"}
        if (((index) %% 7) == 0) {timeRiskfactor <- "4Y"}
      }
    } 
    
    if (type == "forward") { 
      typeOfRiskfactor <- ceiling(index/7) 
      if (typeOfRiskfactor == 1) {nameRiskfactor <- "Aluminium"} 
      if (typeOfRiskfactor == 2) {nameRiskfactor <- "Ingot"}
      if (typeOfRiskfactor == 3) {nameRiskfactor <- "Alumina"}
      if (typeOfRiskfactor == 4) {nameRiskfactor <- "Coke"}
      if (typeOfRiskfactor == 5) {nameRiskfactor <- "Oil"}
      if (typeOfRiskfactor == 6) {nameRiskfactor <- "Caustic"}
      if (typeOfRiskfactor == 7) {nameRiskfactor <- "Pitch"}
      if (typeOfRiskfactor == 8) {nameRiskfactor <- "Coal"}
      if (typeOfRiskfactor == 9) {nameRiskfactor <- "USDNOK"}
      if (typeOfRiskfactor == 10) {nameRiskfactor <- "BRLNOK"}
      if (typeOfRiskfactor == 11) {nameRiskfactor <- "EURNOK"}
        
      if (((index) %% 7) == 1) {timeRiskfactor <- "1M"}
      if (((index) %% 7) == 2) {timeRiskfactor <- "3M"}
      if (((index) %% 7) == 3) {timeRiskfactor <- "6M"}
      if (((index) %% 7) == 4) {timeRiskfactor <- "1Y"}
      if (((index) %% 7) == 5) {timeRiskfactor <- "2Y"}
      if (((index) %% 7) == 6) {timeRiskfactor <- "3Y"}
      if (((index) %% 7) == 0) {timeRiskfactor <- "4Y"}
    }
  nameIndex <- paste(nameRiskfactor,timeRiskfactor)
  allNames <- append(allNames,nameIndex)
  }
  return(allNames)
}
