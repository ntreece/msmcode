MSMsimulationlg <- function(kbar, b, m0, gamma_kbar, sigma, NumDays, n, Excel){
  
  MultipleSimulationslg <- list()
  for( i in 1:n){
    MultipleSimulationslg[[i]] <- MSMsimulation(kbar, b, m0, gamma_kbar, sigma, NumDays)[,kbar+2]
    MultipleSimulationslg.mat <- matrix(unlist(MultipleSimulationslg), ncol = n, byrow = TRUE)
    
    if(Excel == 1){
      file = paste("Log_Returns_Simulation", n , ".csv")
      write.csv(MultipleSimulationslg.mat, file)  
    }
    
  }
  return <- MultipleSimulationslg.mat
}