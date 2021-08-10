##### EXPORT ######################################################################################

source("Code/TC_Export.R")


##### COSMETICS ###################################################################################

source("Code/TC_PCCP.R")


##### save data as Rdata ##########################################################################
save(Systems, Materials, Names, SIM, TC.Norm, Input, excel.file,
     file = "Input/InputReady.Mod1.Rdata")