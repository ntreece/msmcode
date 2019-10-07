# Load Simulation function R file
source("MSMsimulation.R")

# INPUT PARAMETERS
kbar <- 10                    # Number of frequency components
kbar = 12
gamma_kbar <- 0.9             # Controls persistence of the kbar multiplier directly
gamma_kbar = 0.7
b <- 2                        # Controls rate at which persistence changes across remaining multipliers
b = 2
m0 <- 1.5                     # Controls volatility of volatility
m0 = 1.31
sigma <- 0.1                  # Unconditional standard deviation of returns
sigma = SPXstd*sqrt(252)      # Prior period hist vol
sigma = .1744953
NumDays <- 1000               # Number of days 
NumDays = 7599


# OUTPUT RESULTS
Output_MSMSimulation <-  MSMsimulation(kbar, b, m0, gamma_kbar, sigma, NumDays)

FreqComponentMatrix <- Output_MSMSimulation[,1:kbar]
Volatility <- Output_MSMSimulation[,kbar+1]
logReturns <- Output_MSMSimulation[,kbar+2]
Price      <- Output_MSMSimulation[,kbar+3]

# Visual Output
#plot(Volatility, type = "l", lwd = 1, main = "Daily Volatility", ylab = "Volatility", xlab = "Days")
plot(logReturns, type ="l", lwd = 1.5, main = "Simulated Daily Returns", ylab = "Simulated Daily Returns", xlab = "Days", ylim=c(-0.12,0.12))
#plot(Price, type = "l", lwd = 1.5, main = "Daily Asset Price", ylab = "Price", xlab = "Days")

# View result figures in R
#View(Output_MSMSimulation)
#View(FreqComponentMatrix)
#View(Volatility)
#View(logReturns)
#View(Price)


# Export results to Excel file.
write.csv(Output_MSMSimulation, file = "Output_MSMSimulation.csv")
#write.csv(FreqComponentMatrix, file = "FreqComponentMatrix.csv")
#write.csv(Volatility, file = "Volatility.csv")
write.csv(logReturns, file = "logReturns.csv")
write.csv(Price, file = "Price.csv")



############################################## 
# GENERATING MULTIPLE SIMULATIONS AT ONE GO  #
##############################################

# Generate 10 simulations and output in a matrix object and excel file (optional)
# 10 simulation + write in excel file takes around 10 seconds
# 100 simulation takes around 1 minute depending on speed of your computer.

source("MSMsimulation.R")      # Load MSM Simulation function
source("MSMsimulationLgRet.R") # Load MSM log returns simulation function

# Inputs
kbar = 10
b = 2
m0 = 1.4
gamma_kbar = 0.5
sigma = SPXstd*sqrt(252)
NumDays = 11291
n = 10           # Number of simulations
Excel = 0        # '1' means output to Excel file or '0' means no excel file output


#Ouput_MSMsimulationlg <- MSMsimulationlg(kbar, b, m0, gamma_kbar, sigma, NumDays, n, Excel)
#View(Ouput_MSMsimulationlg)
#plot(Ouput_MSMsimulationlg[,2],type="l", xlab = "Days", ylab = "Log Returns" )


source("MSMsimulation.R")
source("MSMsimulationPrice.R")
#Ouput_MSMsimulationPrice <- MSMsimulationPrice(kbar, b, m0, gamma_kbar, sigma, NumDays, n, Excel)
#View(Ouput_MSMsimulationPrice)
#plot(Ouput_MSMsimulationPrice[,6],type = "l", xlab = "Days", ylab = "Price" )

