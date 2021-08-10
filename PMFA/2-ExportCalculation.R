# Script that calculates the magnitude of environmental flows
# =================================================================================================
#                           
# Date of last modification: 17.07.2017
# =================================================================================================

##### INTRO #######################################################################################

# source needed functions
source("D:\WORKSPACE\zipeng\functions.needed.R")
library("xlsx")
library("trapezoid")

# create input data
# source("MatrixCreationv2.R")

# import data
load("D:\WORKSPACE\zipeng\Input\InputFormatted.Rdata")

# import the data
Inp.EU <- as.matrix(read.xlsx("2018test3.xlsx", sheetName = "Input_EU"))
Inp.CH <- as.matrix(read.xlsx("2018test3.xlsx", sheetName = "Input_CH"))
Coeff  <- as.matrix(read.xlsx("2018test3.xlsx", sheetName = "TransferCoefficients"))

# loop over systems and polymers
for(sys in Systems){
  
  # take appropriate data for the system
  if(sys == "EU"){ data <- Inp.EU } else { data <- Inp.CH }
  #data <- Inp.EU
  data <- data[, c("Compartment", "Material", "Data..kt.", "Spread")]
  
  for(mat in Materials){
    message("\n", "......... ", sys, ", ", mat, " .........", "\n")
    
    # loop over compartments
    for(comp in Names){
      
      # calculate export if the input is negative or not undefined
      if(!(is.null(Input[[sys]][[mat]][[comp]]) | all(Input[[sys]][[mat]][[comp]] >= 0))){
        
        # stop if data has positive and negative values
        if(any(Input[[sys]][[mat]][[comp]] < 0) & any(Input[[sys]][[mat]][[comp]] > 0)){
          warning(sys, ", ", mat, ", ", comp, ": ", "The input has both negative and positive values.")
          next
        } 
        
        # calculate the missing TC
        message(paste0(format(Sys.time(), "%H:%M:%S")," Calculating export flow from ", comp, ".")) 
        
        # calculate mean flows
        flo <- lapply(TC.Norm[[sys]][[mat]], function(x) lapply(x,mean))
        inp <- lapply(Input[[sys]][[mat]], function(x){ if(is.null(x)){ 0 } else { mean(x) } })
        inp[inp <= 0] <- 0
        
        inp <- lapply(inp, function(x){ if(x < 0){ 0 } else {x} })
        
        Mass <- solve.MC(TC.Distr  = flo,
                         inp.Distr = inp,
                         Names     = Names,
                         N         = 1)
        
        # calculate export TC
        ex <- abs(as.numeric(data[which(data[,"Compartment"] == comp &
                                          data[, "Material"] == mat), "Data..kt."]))
        TC <- ex/Mass[comp,]
        if(any(TC > 1)){
          warning("The export flow from ", comp, " in ", sys, " for ", mat, " is larger than the mass content. The corresponding export flow is set to 1.")
          TC[TC > 1] <- 1
        }
        
        # # if there is more than one data point, stop calculation (the problem is for the
        # # uncertainty attribution, which one of the two data points should be chosen?)
        # if(length(TC) > 1){
        #   stop("Cannot deal with more than one export data point.")
        # }
        
        # attribute spread from export value
        UC <- as.numeric(data[which(data[,"Compartment"] == comp &
                                      data[, "Material"] == mat), "Spread"])
        
        # inject into new list
        
        # TC.Norm[[sys]][[mat]][[comp]][["Export"]] <- rtriang.perc(mmode = TC,
        #                                                           perc = UC,
        #                                                           linf = 0,
        #                                                           lsup = 1,
        #                                                           N = SIM)
        
        TC.Norm[[sys]][[mat]][[comp]][["Export"]] <- rwrap(values = TC, 
                                                           perc = UC,
                                                           N = SIM, 
                                                           linf = 0,
                                                           lsup = 1)
        
        
        # renormalize
        TC.Norm[[sys]][[mat]] <- normalize(TC.Norm[[sys]][[mat]])
        
        # make sure corresponding negative input is now zero
        Input[[sys]][[mat]][comp] <- list(NULL)
        
        # timer
        message(paste(format(Sys.time(), "%H:%M:%S"),"Done. \n")) 
        
      }
      
     
    }
    
  }
}



##### save data as Rdata ##########################################################################
save(Systems        = Systems,
     Materials      = Materials,
     Names          = Names,
     SIM            = SIM,
     TC             = TC.Norm,
     Input          = Input,
     file           = "Input/InputReady.Rdata")

