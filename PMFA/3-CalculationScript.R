# Script that performs a Monte-Carlo MFA Calculation
# =================================================================================================
#                           
# Date of last modification: 28.06.2017
# =================================================================================================

##### INTRO #######################################################################################

# source needed functions
source("D:\WORKSPACE\zipeng\functions.needed.R")
library("xlsx")
library("trapezoid")

# create input data
# source("MatrixCreationv2.R")

# import data
load("D:\WORKSPACE\zipeng\Input\InputReady.Rdata")

# loop over systems and polymers
for(sys in Systems){
  for(mat in Materials){
    
    timer <- proc.time()
    
    # the solve.MC function will solve the equation system N times after having
    # reconstructed the TC matrix for every iteration
    Mass <- solve.MC(TC.Distr  = TC.Norm[[sys]][[mat]],
                     inp.Distr = Input[[sys]][[mat]],
                     Names     = Names,
                     N         = SIM)
    
    save(Mass = Mass,
         file = paste0("Results/ResultsMass_NoSimplification/OutputMass_",sys,"_",mat,".Rdata"))
    
    # notify how long was needed for the whole calculation
    message("Time needed for the simulation:")
    print(proc.time() - timer) # second timer
  }
}

