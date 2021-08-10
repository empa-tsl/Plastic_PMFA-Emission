##### SPECIAL WW FLOWS ############################################################################

source("Code/TC_WW.R")

##### SPECIAL SWEEPING FLOWS ######################################################################

source("Code/TC_Sweeping.R")

##### TEXTILES RELEASE ############################################################################

source("Code/TC_Textiles.R")

##### LITTERING ###################################################################################

# source("Code/TC_Littering.R")

##### AUTOMOTIVE RELEASE ##########################################################################

source("Code/TC_Automotive.R")

##### COMPOST #####################################################################################

source("Code/TC_Compost.R")

# ##### VERIFICATION ################################################################################
# 
# for(sys in Systems){
#   for(mat in Materials){
#     for(comp in names(TC.Norm[[sys]][[mat]])){
#       
#       if(any(abs(apply(do.call(rbind, TC.Norm[[sys]][[mat]][[comp]]),2,sum) - 1) > 0.0000001)){
#         stop("The flows in ", sys, ", ",mat, " from ",comp," are not normalized")
#       }
#       
#     }
#   }
# }


##### save data as Rdata ##########################################################################
save(Systems, Materials, Names, SIM, TC.Norm, Input,  excel.file,
     file = "Input/InputReady.Mod2.Rdata")
