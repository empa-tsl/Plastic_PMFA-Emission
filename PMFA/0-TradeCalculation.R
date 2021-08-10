library(xlsx)
Materials <- c("PUR", "ABS", "PA", "PC", "PMMA")

##### Importing the trade data for Europe #########################################################

Imp <- as.matrix(read.xlsx("~/Data collection/Module 1/Trade/TOTAL_EU.xlsx", sheetName = "Import",
                           startRow = 12, endRow = 226))
Exp <- as.matrix(read.xlsx("~/Data collection/Module 1/Trade/TOTAL_EU.xlsx", sheetName = "Export",
                           startRow = 12, endRow = 226))
# formatting
Imp[Imp == ":"] <- NA
Exp[Exp == ":"] <- NA
rownames(Imp) <- Imp[,1]
rownames(Exp) <- Exp[,1]
Imp <- Imp[,-1]
Exp <- Exp[,-1]
Imp <- apply(Imp, c(1,2), as.numeric)
Exp <- apply(Exp, c(1,2), as.numeric)
# calculate net import
Net <- Imp-Exp
Net2014.EU <- Net[,"Jan..Dec..2014"]

##### Importing the trade data for Switzerland ####################################################

Trade <- as.matrix(read.xlsx("~/Data collection/Module 1/Trade/TOTAL_CH.xlsx", sheetName = "2014",
                           startRow = 6, endRow = 350))
rownames(Trade) <- gsub("\\.","",unlist(lapply(strsplit(Trade[,1], " - "), `[[`, 1)))
# format
Trade <- Trade[,-1]
Trade <- apply(Trade, c(1,2), as.numeric)
colnames(Trade) <- c("Import", "Export")
# calculate net import
Net2014.CH <- Trade[,"Import"] - Trade[,"Export"]

##### Importing the attribution table #############################################################

Attr <- as.matrix(read.xlsx("~/Data collection/Module 1/Trade/For Qie/Codes_and_attribution.xlsx",
                            sheetName = "Trade_Attribution"))
# format
codes <- Attr[,2]
rownames(Attr) <- codes
codes <- codes[-1]
Attr <- Attr[,-c(1:4)]
# compartments for which there is an input
comps <- colnames(Attr)[!grepl("NA",colnames(Attr))]
# change column names
for(nam in 1:ncol(Attr)){
  if(grepl("NA", colnames(Attr)[nam])){
    colnames(Attr)[nam] <- paste0(strsplit(colnames(Attr)[nam-1],"\\.")[[1]][1],".",Attr[1,nam])
  } else {
    colnames(Attr)[nam] <- paste0(strsplit(colnames(Attr)[nam],"\\.")[[1]][1],".",Attr[1,nam])
  }
}
# format
Attr <- Attr[-1,]
Attr <- apply(Attr, c(1,2), as.numeric)

# check that all the codes are present
codes.OI <- codes[apply(Attr,1,sum) != 0]
if(!all(codes.OI %in% names(Net2014.EU))){
  cat("These codes are missing from the EU trade data:")
  codes[!codes.OI]
}
if(!all(codes.OI %in% names(Net2014.CH))){
  cat("These codes are missing from the CH trade data:")
  codes.OI[!codes.OI %in% names(Net2014.CH)]
}

##### calculate plastic trade #####################################################################

Trade.2014.EU <- Trade.2014.CH <- matrix(0, length(comps), length(Materials),
                                         dimnames = list(comps, Materials))

for(comp in comps){
  for(mat in Materials){
    
    if(paste0(comp,".",mat) %in% colnames(Attr)){
      
      Trade.2014.EU[comp,mat] <- sum(Net2014.EU[codes] * as.numeric(Attr[codes,paste0(comp,".",mat)]),
                                  na.rm = T)/10/1000
      
      Trade.2014.CH[comp,mat] <- sum(Net2014.CH[codes] * as.numeric(Attr[codes,paste0(comp,".",mat)]),
                                  na.rm = T)/1000
      
    } else {
      next
    }

  }
}


# ----------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------- #

rm(list = ls())
Materials <- c("PUR", "ABS", "PA", "PC", "PMMA")

##### packaging europe ############################################################################

Imp <- as.matrix(read.xlsx("~/Data collection/Module 1/Trade/PACKAGING_EU.xlsx", sheetName = "Import",
                           startRow = 12, endRow = 66))
Exp <- as.matrix(read.xlsx("~/Data collection/Module 1/Trade/PACKAGING_EU.xlsx", sheetName = "Export",
                           startRow = 12, endRow = 66))
# formatting
Imp[Imp == ":"] <- NA
Exp[Exp == ":"] <- NA
rownames(Imp) <- Imp[,1]
rownames(Exp) <- Exp[,1]
Imp <- Imp[,-1]
Exp <- Exp[,-1]
Imp <- apply(Imp, c(1,2), as.numeric)
Exp <- apply(Exp, c(1,2), as.numeric)
# calculate net import
Net <- Imp-Exp
Net2014.EU <- Net[,"Jan..Dec..2014"]

##### packaging switzerland #######################################################################

Trade <- as.matrix(read.xlsx("~/Data collection/Module 1/Trade/PACKAGING_CH.xlsx", sheetName = "2014",
                             startRow = 7, endRow = 106))
rownames(Trade) <- gsub("\\.","",unlist(lapply(strsplit(Trade[,1], " - "), `[[`, 1)))
# format
Trade <- Trade[,-1]
Trade <- apply(Trade, c(1,2), as.numeric)
colnames(Trade) <- c("Import", "Export")
# calculate net import
Net2014.CH <- Trade[,"Import"] - Trade[,"Export"]

##### Importing the attribution table #############################################################

Attr <- as.matrix(read.xlsx("~/Data collection/Module 1/Trade/For Qie/Codes_and_attribution.xlsx",
                            sheetName = "Trade_Packaging"))
# format
codes <- Attr[,1]
rownames(Attr) <- codes
codes <- codes[-1]
Attr <- Attr[,-1]
# change column names
for(nam in 1:ncol(Attr)){
  if(grepl("NA", colnames(Attr)[nam])){
    colnames(Attr)[nam] <- paste0(strsplit(colnames(Attr)[nam-1],"\\.")[[1]][1],".",Attr[1,nam])
  } else {
    colnames(Attr)[nam] <- paste0(strsplit(colnames(Attr)[nam],"\\.")[[1]][1],".",Attr[1,nam])
  }
}
# format
Attr <- Attr[-1,]
Attr <- apply(Attr, c(1,2), as.numeric)

# check that all the codes are present
if(!all(codes %in% names(Net2014.EU))){
  cat("These codes are missing from the EU trade data:")
  codes[!codes %in% names(Net2014.EU)]
}
if(!all(codes %in% names(Net2014.CH))){
  cat("These codes are missing from the CH trade data:")
  codes[!codes %in% names(Net2014.CH)]
}

##### calculate plastic trade #####################################################################

Trade.2014.EU <- Trade.2014.CH <- matrix(0, 2, length(Materials), 2,
                                         dimnames = list(c("First", "Second"), Materials))
for(est in c("First", "Second")){
  for(mat in Materials){
    
      Trade.2014.EU[est,mat] <- sum(Net2014.EU[codes] * as.numeric(Attr[codes,paste0(est,".",mat)]),
                                     na.rm = T)/10/1000
      
      Trade.2014.CH[est,mat] <- sum(Net2014.CH[codes] * as.numeric(Attr[codes,paste0(est,".",mat)]),
                                     na.rm = T)/1000

    
  }
}
