# Main script
optimera <- function(startDate, endDate, optType, commission, lambda, cash, instrument, months, dataFile, kappa, saveCash, percentExposure, workingDirectory, amplDirectory) {
  startTime <- Sys.time()
  
  setwd(workingDirectory)
  
  # Load libraries
  library("readxl")
  library(openxlsx)
  library(lubridate)
  library(vars)
  library(mvtnorm)
  library(QRM)
  library(fGarch)
  library(copula)
  library(VC2copula)
  library(VineCopula)
  library(lubridate)
  library(plotly)
  
  # Source functions
  source("optimizationFunctions.R")
  source("functions.R")
  source("optIterate.R")
  
  # Makes the seed random for every new run
  rm(.Random.seed, envir=globalenv())
  
  # Start and end date of simulation
  # GUI
  startDate = as.Date(startDate)
  endDate   = as.Date(endDate)
  
  # Set parameters
  commission <- commission/100        # Commission, quoted as a percentage, GUI
  kappa      <- rep(c(kappa), times=11)
  beta       <- 0.95        # Beta
  nu0        <- cash
  n          <- 2000        # Number of scenaro
  D          <- 360         # Day-count convention
  
  # Time to maturity for aluminium, USDNOK and optimization contracts
  tauIVAluminium <- c(1/12, 2/12, 3/12, 6/12, 9/12, 1, 2, 3)
  tauIVUSDNOK    <- c(1/12, 3/12, 6/12, 1, 2, 3, 4)
  tau            <- c(1/12, 3/12, 6/12, 1, 2, 3, 4)
  tauOIS         <- c(1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12, 10/12, 11/12, 1, 2, 3, 4)
  tauFuture      <- seq(1/12, 4, by=1/12)
  
  # Moneyness for aluminium and delta for USDNOK
  moneynessAluminium <- c(0.8, 0.85, 0.90, 0.95, 1, 1.05, 1.1, 1.15, 1.2)
  deltaUSDNOK        <- c(10, 15, 20, 25, 30, 35, 40, 45)
  
  # Exposure [Aluminium, Ingot, Alumina, Coke, Oil, Caustic, Pitch, Coal, UsdNok, BrlNok, EurNok]
  exposure <- c(1.76, 2.2, 2.46, -0.89, -0.63, -0.56, -0.17, -0.72, -351.7, -3205.13, 97.47)*10^6/12

  
  
  # ---------- Start of main script ----------
  
  oldContracts <- list(data.frame(),data.frame(),data.frame(),data.frame(),data.frame())
  today <- startDate
  while (today <= endDate) {
    print(today)
    oldContracts <- opt(workingDirectory, n, D, commission, tau, tauIVAluminium, tauIVUSDNOK,
                        tauOIS, tauFuture, exposure, kappa, lambda, nu0, cash, beta,
                        moneynessAluminium, deltaUSDNOK, today, startDate, oldContracts,
                        optType, p_list, type, instrument, months, saveCash, 
                        percentExposure, dataFile, amplDir = amplDirectory)
    nu0 <- oldContracts[[4]][match(today,oldContracts[[4]]$today),4]
    cash <- oldContracts[[5]][match(today,oldContracts[[5]]$today),4]
    today <- ymd(today) %m+% months(1)
    print(nu0)
  }
  
  # Statistical test
  statTestValue <- statisticalTest(oldContracts[[4]],0.05)
  statTestObjective <- statisticalTest(oldContracts[[5]],0.05)
  
  # Plots
  par(mfrow=c(1,2))
  date <- oldContracts[[4]][["today"]]
  
  # Value of hedged portfolio against unhedged
  valueHedged <- oldContracts[[4]][["objHedged"]]
  valueUnhedged <- oldContracts[[4]][["objUnhedged"]]
  fig1 <- plot_ly(x=date, y=valueHedged, type = 'scatter', mode="lines", name="Hedged Value")
  fig1 <- fig1 %>% add_trace(y=valueUnhedged, mode="lines", name="Unhedged Val")
  if (dim(oldContracts[[4]])[2]==5) {
    valueTransCost <- oldContracts[[4]][["hedged"]]
    fig1 <- fig1 %>% add_trace(y=valueTransCost, mode="lines", name="Hedged not TransCost")
  }
  fig1 <- fig1 %>% layout(title = "Portfolio value", xaxis = list(title = "Date"), yaxis = list(title = "Value, MNOK"))
  
  # Objective value of hedged portfolio against unhedged
  objHedged <- oldContracts[[5]][["objHedged"]]
  objUnhedged <- oldContracts[[5]][["objUnhedged"]]
  fig2 <- plot_ly(x=date, y=objHedged, type = 'scatter', mode="lines", name="Hedged Obj")
  fig2 <- fig2 %>% add_trace(y=objUnhedged, mode="lines", name="Unhedged Obj")
  fig2 <- fig2 %>% layout(title = "Objective value", xaxis = list(title = "Date"), yaxis = list(title = "Value, MNOK"))
  
  fig <- subplot(fig1, fig2)
  print(fig)
  
 
  folderName <- paste(optType,
                      "Kappa",as.character(kappa[1]),
                      "Lambda", as.character(lambda),
                      "From",as.character(startDate),
                      "To", as.character(endDate),
                      "With", paste(unlist(instrument), collapse=','),  
                      sep="")
  
  dir.create(folderName)
  
  putClean <- data.frame(amount=oldContracts[[1]][["amount"]], strike=oldContracts[[1]][["strike"]]*10^6, timeToMat=oldContracts[[1]][["timeToMat"]], startDate=as.Date(oldContracts[[1]][["startDate"]]), whichContract=oldContracts[[1]][["whichContract"]])
  longClean <- data.frame(amount=oldContracts[[2]][["amount"]], strike=oldContracts[[2]][["strike"]]*10^6, timeToMat=oldContracts[[2]][["timeToMat"]], startDate=as.Date(oldContracts[[2]][["startDate"]]), whichContract=oldContracts[[2]][["whichContract"]])
  shortClean <- data.frame(amount=oldContracts[[3]][["amount"]], strike=oldContracts[[3]][["strike"]]*10^6, timeToMat=oldContracts[[3]][["timeToMat"]], startDate=as.Date(oldContracts[[3]][["startDate"]]), whichContract=oldContracts[[3]][["whichContract"]])
  portfolioClean <- data.frame(objHedged=oldContracts[[4]][["objHedged"]], objUnhedged=oldContracts[[4]][["objUnhedged"]], today=as.Date(oldContracts[[4]][["today"]]), nu=oldContracts[[4]][["nu..1...1."]])
  objectiveClean <- data.frame(objHedged=oldContracts[[5]][["objHedged"]], objUnhedged=oldContracts[[5]][["objUnhedged"]], today=as.Date(oldContracts[[5]][["today"]]), cash=oldContracts[[5]][["cash...exp.r.12."]])
  
  write.xlsx(putClean,paste(folderName,"/Put.xlsx",sep=""))
  write.xlsx(longClean,paste(folderName,"/Long.xlsx",sep=""))
  write.xlsx(shortClean,paste(folderName,"/Short.xlsx",sep=""))
  write.xlsx(portfolioClean,paste(folderName,"/Portfolio.xlsx",sep=""))
  write.xlsx(objectiveClean,paste(folderName,"/Objective.xlsx",sep=""))
  
  
  png(paste(folderName,"/graphs.png",sep=""))
  
  par(mfrow=c(1,2))
  
  plot(x=date, y=valueHedged, main="Portfolio value",
       type="l", col="yellow", xlab="Date", ylab="Value",
       ylim=c(min(valueHedged,valueUnhedged), max(valueHedged,valueUnhedged)))
  lines(x=date, y=valueUnhedged, col="blue")
  legend("topleft",legend=c("Hedged", "Unhedged"),
         col=c("yellow","blue"),lty=1:2, cex=0.8)
  
  plot(x=date, y=objHedged, main="Objective value", 
       type="l", col="red", xlab="Date", ylab="Value",
       ylim=c(min(objHedged,objUnhedged), max(objHedged,objUnhedged)))
  lines(x=date, y=objUnhedged, col="green")
  legend("topleft",legend=c("Hedged", "Unhedged"),
         col=c("red","green"),lty=1:2, cex=0.8)
  
  dev.off()
  
  oldContracts[[1]][["startDate"]] <- as.Date(oldContracts[[1]][["startDate"]])
  oldContracts[[2]][["startDate"]] <- as.Date(oldContracts[[2]][["startDate"]])
  oldContracts[[3]][["startDate"]] <- as.Date(oldContracts[[3]][["startDate"]])
  
  # Evaluates runtime of program
  endTime <- Sys.time()
  print(endTime-startTime)
  print("Put options bought, MNOK")
  print(oldContracts[[1]])
  print("Long forwards bought, MNOK")
  print(oldContracts[[2]])
  print("Short forwards bought, MNOK")
  print(oldContracts[[3]])
  
  
  Put <- data.frame(Instrument=indexToName("option",oldContracts[[1]][["whichContract"]]), Strike=oldContracts[[1]][["strike"]]*10^6, Start=as.character(oldContracts[[1]][["startDate"]]), Amount=oldContracts[[1]][["amount"]])
  Long <- data.frame(Instrument=indexToName("forward",oldContracts[[2]][["whichContract"]]), Start=as.character(oldContracts[[2]][["startDate"]]), Amount=oldContracts[[2]][["amount"]])
  Short <- data.frame(Instrument=indexToName("forward",oldContracts[[3]][["whichContract"]]), Start=as.character(oldContracts[[3]][["startDate"]]), Amount=oldContracts[[3]][["amount"]])
  toReturn <- list(Put, Long, Short, statTestValue, statTestObjective)
  return(toReturn)
}
