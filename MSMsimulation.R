######################################################
# Author: Samuel Jeeban
# Date : Oct 2014
# Version: 1.0
# Copyright 2014
# This scrip is the R code for MSM simulation based on excel sheet 
# provided by Fisher and Calvet (2004) at
# https://www.google.mu/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&cad=rja&ved=0CDIQFjAB&url=https%3A%2F%2Fstudies2.hec.fr%2Fjahia%2Fwebdav%2Fsite%2Fhec%2Fshared%2Fsites%2Fcalvet%2Facces_anonyme%2FMSM_sim_v1.xls&ei=7PzkUvLoA8bm7Aae14DgDQ&usg=AFQjCNEDg-umD9lZiQ-bhwXip6JeO56gCA&bvm=bv.59930103,d.bGQ


# FUNCTION
MSMsimulation <- function(kbar, b, m0, gamma_kbar, sigma, NumDays){
  
  # Kbar : Number of frequency components (or multipliers) used to calculate volatility.
  # b & gamma_kbar : Control the level of persistence across the multipliers.
  # b : Controls the rate at which the persistence changes across the remaining multipliers.
  # gamma_kbar : Controls the persistence of the k multiplier directly.
  # m0 : Controls the volatility of volatility.
  # sigma : Unconditional annualised standard deviation of the log returns.
  
  
  M    <- matrix(0, ncol = kbar, nrow = NumDays) # Zeros Matrix to hold frequency components.
  Vlty <- matrix(0, ncol = 1, nrow = NumDays)    # Zeros Matrix to hold Volatility.
  lg   <- matrix(0, ncol = 1, nrow = NumDays)    # Zeros Matrix to hold Log Returns. 
  S    <- matrix(0, ncol = 1, nrow = NumDays)    # Zeros Matrix to hold Price.
  S0   <- 100                                    # Set Initial Price as 100.
  
  ##############################################
  # FREQUENCY COMPONENTS and TRANSITION MATRIX #
  ##############################################
  
  for(j in 2:kbar){          # Column of frequency components matrix runs from 1 to kbar.
    for(i in 2:NumDays){     # Rows of frequency components matrix runs from 1 day to NumDays.
      
      
      # TRANSITION PROBABILITIES
      # From Calvet and Fisher 2004 p.53 last paragraph:
      # gamma_kbar= 1-(1-gamma_1)^(b^(k-1)) 
      # Make gamma_1 subject of formula to obtain the below:
      
      gamma_1 <- 1-(1-gamma_kbar)^(1/(b^(kbar-1))) 
      
      g <- (NULL)
      g[1] <- gamma_1
      if(kbar >= j){
        g[j] <- 1-(1-gamma_1)^(b^(j-1))  # gamma_kbar= 1-(1-gamma_1)^(b^(k-1)) 
      }
      
      
      # pc below means part code. It is a code that repeats each time, so I 
      # converted  it into a function to reduce my other codes. 
      # Besides since it is a function, each time I run the function, 
      # the random generator is activated.
      # It is also the first row, first column of the frequency component matrix.
      # It returns either m0 or 2-m0. It is the value that the first frequency 
      # component will take. The value of the next frequency component will be
      # same as the first one or take a new value.
      
      
      pc <- function(x){
        if(runif(1) < 0.5){  # Independent draws
          m0} else {2-m0}
      }
      
      
      # M[1,1] is the first row, first column cell in the frequency component matrix.
      # To get value in cell M[1,1] for Day 1, it's: 
      # if random generator is less than 0.5 either we get m_0  or 2-m_0.
      
      # So the binomial distribution is obvious here. We are choosing from 2 values
      # m0 or 2-m0 and it is done randomly so each draw is independent of the other
      # following Calvet and Fisher 2004 p.53, where it states that "new draws 
      # from M are assumed to be independent across k and t."
      
      M[1,1] <- pc()
      
      
      # All frequency component (or multipliers), k,  for Day = 1 only
      # First row of the frequency component matrix
      
      if(kbar >= j) {
        if(runif(1) < 0.5){      # Independent draws
          M[1,j] <- m0           
        } else M[1,j] <- 2- m0
      }
      
      
      # Frequency component 1 only for Day = 2 till Day = NumDays
      # First column of the frequency component matrix
      
      # Calvet and Fisher 2004, p.53:
      # The dynamics of M_(k,t) can be summarised as:
      # M_(k,t) drawn from distribution M with probability gamma_k.
      # M_(k,t) = M_(k,t-1)  with probability 1-gamma_k.
      
      # So the code below is, if randomly generated number is less than gamma_1
      # then the cell M[i,1] is either m0 or 2-m0 which is given by pc().
      # Else we use the value from the previous state.
      
      if(runif(1) < g[1]){
        M[i,1] <- pc()             # M_(k,t)
      } else {M[i,1] <- M[i-1,1]   # M_(k,t) = M_(k,t-1)
      }
      
      
      # Frequency component 2 to kbar for Day = 2 till Day = NumDays
      # Second till kbar-th column of the frequency component matrix
      
      if(kbar >= j){                # Test first if have more than 1 volatility component
        if(runif(1) < g[j] ){  
          M[i,j] <- pc()            # M_(k,t)
        } else {M[i,j] <- M[i-1,j]} # M_(k,t) = M_(k,t-1)
      }
      
      
    }
  }
  
  ##############################################
  # Volatility  and Log Returns                #
  ##############################################
  
  # Volatility is defined in Calvet and Fisher 2004 p.53 as:
  # sigma_t = sigma * [ M_{1,t} * M_{2,t} * ... * M_{kbar,t}]^(0.5)
  
  # Where sigma_t is the volatility at time t,
  # sigma is equal to the unconditional standard deviation of the innovation x_t.
  # M_{1,t} * M_{1,t} * ... * M_{1,t} are the random multipliers or volatility components
  
  # Log return Column
  # Returns in Fisher and Calvet is defined as:
  # x_t = sigma * [M_{1,t} * M_{2,t} * ... * M_{kbar,t}]^(0.5) * varepsilon_t 
  
  # Where x_t is the returns at time t. [x_t=ln(P_t/P_{t-1})]
  # sigma and the random multipliers defined as above
  # and varepsilon_t are i.i.d. standard Gaussians N(0,1)
  
  for(i in 1:NumDays){
    # sigma_t = sigma * [ M_{1,t} * M_{2,t} * ... * M_{kbar,t}]^(0.5)
    {Vlty[i] <-  (sigma/sqrt(252)) * (prod(M[i,]))^0.5} # Volatility day i
    
    # x_t = sigma * [M_{1,t} * M_{2,t} * ... * M_{kbar,t}]^(0.5) * varepsilon_t
    lg[i] <- Vlty[i] * rnorm(1)                       # Centered log returns day i
  }
  
  #Three vol frameworks: MSM, MSM with no vol of vol, and Normal
  #(prod(M[i,]))^0.5
  #(m0^(kbar/2)*(2-m0)^(kbar/2))^.5
  #1
  
  ##############################################
  # Asset Price                                #
  ##############################################
  
  for(i in 2:NumDays){
    S[1] <- S0*exp(lg[1])
    S[i] <- S[i-1]*exp(lg[i])
  }
  
  return <- cbind(M,Vlty,lg,S)
  
}
