##### INTRO #######################################################################################

# import the initial data
Inp.CH <- as.matrix(read.xlsx(paste0("Input/",excel.file), sheetName = "Input"))
Coeff1 <- as.matrix(read.xlsx(paste0("Input/",excel.file), sheetName = "Module1"))
Coeff2 <- as.matrix(read.xlsx(paste0("Input/",excel.file), sheetName = "Module2"))

# find all the compartment names
Names <- unique(c(Coeff1[-1,c(1,2)], Coeff2[-1,c(1,2)]))

##### INPUT #######################################################################################

Input <- import.input(Inp.CH, Materials)

#message(paste(format(Sys.time(), "%H:%M:%S"), "Input formatting complete."))

##### TC MODULE 1 #################################################################################

TC <- import.TC(Coeff1, Materials)
TC <- calc.rest.TC(TC)

# normalization step: make sure that all the flows going out of a compartment sum up to 1
TC.Norm <- sapply(Materials, function(x) NULL)
for(mat in Materials){
  TC.Norm[[mat]] <- normalize(TC[[mat]])
}

message(paste(format(Sys.time(), "%H:%M:%S"), "TC module 1 formatting complete.")) 

##### TC MODULE 2 #################################################################################

TC.Release <- import.TC(Coeff2, Materials)

message(paste(format(Sys.time(), "%H:%M:%S"), "TC module 2 formatting complete.")) 

##### SAVE DATA ###################################################################################

# save formatted data as Rdata
save(Materials, Names, SIM, TC.Norm, TC.Release, Input, excel.file,
     file = "Input/InputFormatted.Rdata")

# save unnormalized TCs
# save(TC, 
#      file = "Input/InputUnnormalized.Rdata")

message(paste(format(Sys.time(), "%H:%M:%S"), "Unmerged data saved in Input/InputFormatted.Rdata"))

