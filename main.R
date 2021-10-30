# Main script

# Set Working directory
workingDirectory <- "//ad.liu.se/home/eriha203/Documents/Exjobb/git/exjobb/"
amplDirectory <- "//ad.liu.se/home/eriha203/Documents/Exjobb/ampl"

setwd(workingDirectory)

startTime <- Sys.time()

# Load the data from Erik and Victor
load("param_scenario_generation.RData")

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
startDate = as.Date("2018-03-01")
endDate   = as.Date("2018-04-01")

# Set parameters
commission <- 0.02        # Commission, quoted as a percentage, GUI
lambda     <- 0.5         # Lambda
kappa      <- rep(c(10000),times=11)
beta       <- 0.99        # Beta
nu0        <- 15011       # Cash MNOK
n          <- 100         # Number of scenario
D          <- 360         # Day-count convention
cash       <- nu0

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

# Sets objective function to "variance", "cvar" or "utility"
optType <- "variance"

instrument <- c("aluminium", "ingot", "alumina", "coke", "oil", "caustic", "pitch", "coal", "usdnok", "brlnok", "eurnok")

plotCash <- list()
oldContracts <- list(data.frame(),data.frame(),data.frame(),data.frame(),data.frame())


today <- startDate
temp_months <- 48
while (today <= endDate) {
  print(today)
  oldContracts <- opt(workingDirectory, n, D, commission, tau, tauIVAluminium, tauIVUSDNOK,
                      tauOIS, tauFuture, exposure, kappa, lambda, nu0, cash, beta,
                      moneynessAluminium, deltaUSDNOK, today, startDate, oldContracts,
                      optType, p_list, type, instrument, months = temp_months, 
                      saveCash = 100, percentExposure = 100, amplDir <- amplDirectory)
  nu0 <- oldContracts[[4]][match(today,oldContracts[[4]]$today),4]
  cash <- oldContracts[[5]][match(today,oldContracts[[5]]$today),4]
  today <- ymd(today) %m+% months(1)
  plotCash <- c(plotCash,nu0)
  print(nu0)
}


par(mfrow=c(2,2))
date <- oldContracts[[4]][["today"]]

# Value of hedged portfolio against unhedged
valueHedged <- oldContracts[[4]][["objHedged"]]
valueUnhedged <- oldContracts[[4]][["objUnhedged"]]
minY <- min(c(valueHedged,valueUnhedged))
maxY <- max(c(valueHedged,valueUnhedged))
fig1 <- plot_ly(x=date, y=valueHedged, type = 'scatter', mode="lines", name="Hedged Value")
fig1 <- fig1 %>% add_trace(y=valueUnhedged, mode="lines", name="Unhedged Val")
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
                    "To",as.character(endDate),
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



# Evaluates runtime of program
endTime <- Sys.time()
print(endTime-startTime)
print("Put options bought, MNOK")
print(oldContracts[[1]])
print("Long forwards bought, MNOK")
print(oldContracts[[2]])
print("Short forwards bought, MNOK")
print(oldContracts[[3]])
