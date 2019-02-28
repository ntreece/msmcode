MSMsimulationPrice <- function(kbar, b, m0, gamma_kbar, sigma, NumDays, n, Excel){
  
  MultipleSimulationsPrice <- list()
  for( i in 1:n){
    MultipleSimulationsPrice[[i]] <- MSMsimulation(kbar, b, m0, gamma_kbar, sigma, NumDays)[,kbar+3]
    MultipleSimulationsPrice.mat <- matrix(unlist(MultipleSimulationsPrice), ncol = n, byrow = TRUE)
    
    if(Excel == 1){
      file = paste("AssetPrice_Simulation", n , ".csv")
      write.csv(MultipleSimulationsPrice.mat, file)  
    }
    
  }
  return <- MultipleSimulationsPrice.mat
}