##### INTRO #######################################################################################
setwd("D:\WORKSPACE\zipeng")
Materials <- c("PUR", "ABS", "PA", "PC", "PMMA")
# Materials <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
Systems <- c("EU", "CH")
SIM <- 10^5

source("D:\WORKSPACE\zipeng\functions.needed.R")
library("xlsx")

# import the initial data
Inp.EU <- as.matrix(read.xlsx("2018test3.xlsx", sheetName = "Input_EU"))[,1:11]
Inp.CH <- as.matrix(read.xlsx("2018test3.xlsx", sheetName = "Input_CH"))
Coeff <- as.matrix(read.xlsx("2018test3.xlsx", sheetName = "TransferCoefficients"))

# find all the compartment names
Names <- unique(c(Coeff[-1,c(1,2)]))

message(paste(format(Sys.time(), "%H:%M:%S"), "Data importation complete."))

##### INPUT #######################################################################################



Input <- import.input(Inp.EU, Inp.CH, Systems, Materials)

message(paste(format(Sys.time(), "%H:%M:%S"), "Input formatting complete."))

##### TC ##########################################################################################

TC <- import.TC(Coeff, Systems, Materials)
TC <- calc.rest.TC(TC)
message(paste(format(Sys.time(), "%H:%M:%S"), "TC formatting complete.")) 

##### NORMALIZE ###################################################################################

TC.Norm <- sapply(Systems, function (x) NULL)
TC.Norm[["EU"]] <- TC.Norm[["CH"]] <- sapply(Materials, function(x) NULL)

# normalization step: make sure that all the flows going out of a compartment sum up to 1
for(sys in Systems){
  message(paste(format(Sys.time(), "%H:%M:%S"), sys)) 
  for(mat in Materials){
    TC.Norm[[sys]][[mat]] <- normalize(TC[[sys]][[mat]])
  }
}


##### SAVE DATA ###################################################################################

# save data as Rdata
save(Systems        = Systems,
     Materials      = Materials,
     Names          = Names,
     SIM            = SIM,
     TC             = TC.Norm,
     Input          = Input,
     file           = "Input/InputFormatted.Rdata")

# save data as Rdata
save(TC.prenorm     = TC,
     file           = "Input/InputUnnormalized.Rdata")

message(paste(format(Sys.time(), "%H:%M:%S"), "Data saved in Input/InputFormatted.Rdata"))
