# Script that performs a Monte-Carlo MFA Calculation
# =================================================================================================
#                           
# Date of last modification: 28.06.2017
# =================================================================================================

##### INTRO #######################################################################################

# source needed functions
source("Code/functions.needed.R")
library("xlsx")
library("trapezoid")

# create input data
# source("MatrixCreationv2.R")

# import data
load("Input/InputReady.Mod2.Rdata")

# loop over systems and polymers
for(mat in Materials){
  
  timer <- proc.time()
  
  # the solve.MC function will solve the equation system N times after having
  # reconstructed the TC matrix for every iteration
  Mass <- solve.MC(TC.Distr  = TC.Norm[[mat]],
                   inp.Distr = Input[[mat]],
                   Names     = Names,
                   N         = SIM)
  
  save(Mass = Mass,
       file = paste0("Results/ResultsMass_NoSimplification/OutputMass_",mat,".Rdata"))
  
  # notify how long was needed for the whole calculation
  message("Time needed for the simulation:")
  print(proc.time() - timer) # second timer
}


