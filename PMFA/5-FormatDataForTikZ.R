# import input data
setwd("D:\WORKSPACE\zipeng\code1210")
load("Input/InputReady.Rdata")
source("test.R")
# create empty vectors for flows
Flow <- sapply(Systems, function (x) NULL)
Flow[["EU"]] <- Flow[["CH"]] <- sapply(Materials, function(x) NULL)
for(mat in Materials){
  Flow[["EU"]][[mat]] <- Flow[["CH"]][[mat]] <- sapply(Names, function(x) NULL)
}

for(sys in Systems){
  for(mat in Materials){
    print(sys)
    # import output data
    load(paste0("Results/ResultsMass_NoSimplification/OutputMass_",sys,"_",mat,".Rdata"))
    
    ##### UNSIMPLIFIED MASSES #####
    
    # Format SD and MEAN appropriately
    MEAN <- apply(Mass, 1, mean)
    SD <- apply(Mass, 1, function(x) signif(sd(x),1))
    MEAN <- round(MEAN,-sapply(log(SD,10), function(x){ if(is.na(x)){NA} else if(!is.finite(x)){0} else {floor(x)}}))
    # MEAN <- sapply(Mass.S, function(x) signif(mean(x),3))
    # SD <- sapply(Mass.S, function(x) signif(sd(x),2))
    
    # bar length attribution
    BW <- MEAN/max(MEAN)*2.8
    BW[BW < 0.006 & BW != 0] <- 0.006
    
    # save data for further processing
    write(paste0(rownames(Mass), ", ", MEAN, "$\\pm$", SD, ", ", round(BW,digits=2)),
          file = paste0("Charts/2017.10.20 Advanced Charts (TikZ)/mass",sys,mat,".txt"))
    
    ##### SIMPLIFIED MASSES #####
    
    Mass.S <- sapply(c("Production", "Manufacturing", "Import", "Packaging", "BuildingConstruction",
                       "Automotive", "EEE", "Agriculture", "ApparelHH", "TechnicalTextiles", "Other", "Export",
                       "MixedCollection", "WasteCollection", "Recycling", "Landfill", "Incineration",
                       "WWTP", "Reuse", "Elimination"), function(x) NULL)
    
    # masses where the compartments have not changed
    for(comp in c("Packaging", "BuildingConstruction", "Agriculture",
                  "Other", "TechnicalTextiles", "MixedCollection",
                  "Landfill", "Incineration", "WWTP", "Elimination", "Export")){
      Mass.S[[comp]] <- Mass[comp,]
    }
    
    # mass of production
    Mass.S[["Production"]] <- ( Mass["PrimaryProduction",] +
                                  Mass["RecyclateRepelletizing",] )
    # mass of manufacturing
    Mass.S[["Manufacturing"]] <- ( Mass["NonTextileManufacturing",] +
                                   Mass["TextileManufacturing",] )
    
    # mass of special sectors
    Mass.S[["Automotive"]] <- ( Mass["AutomotivePC",] )
    
    Mass.S[["EEE"]] <- ( Mass["EEEPC",] )
    
    Mass.S[["ApparelHH"]] <- ( Mass["ApparelPC",] +
                               Mass["HouseholdTextilesPC",] )
    
    # mass of separate waste collection
    Mass.S[["WasteCollection"]] <- ( Mass["PackagingCollection",] +
                                     Mass["CDCollection",] +
                                     Mass["CDIncinerableCollection",] +
                                     Mass["AgricultureCollection",] +
                                     Mass["ELVCollection",] +
                                     Mass["ELVTextilesCollection",] +
                                     Mass["WEEECollection",] +
                                     Mass["TextileCollection",] +
                                     Mass["PCCollection",] )
    
    # mass in recycling
    Mass.S[["Recycling"]] <- ( Mass["PackagingRecycling",] +
                               Mass["CDRecycling",] +
                               Mass["AgricultureRecycling",] +
                               Mass["LargePartRecycling",] +
                               Mass["ASRRecycling",] +
                               Mass["WEEPRecycling",] )
    
    # mass in reuse
    Mass.S[["Reuse"]] <- ( Mass["TextileReuse",] +
                           Mass["MaterialReuse",] +
                           Mass["PartReuse",] )
    # mass in import (sum input over compartments that are not empty and that are not "PrimaryProduction" or "RecyclateRepelletizing")
    
    Mass.S[["Import"]] <- apply(do.call("rbind",
                                        Input[[sys]][[mat]][-which(names(Input[[sys]][[mat]]) %in% c("PrimaryProduction",
                                               "RecyclateRepelletizing"))]),
                                2,sum)
    
    # Format SD and MEAN appropriately
    MEAN <- sapply(Mass.S, mean)
    SD <- sapply(Mass.S, function(x) signif(sd(x),1))
    MEAN <- round(MEAN,-sapply(log(SD,10), function(x){ if(is.na(x)){NA} else if(!is.finite(x)){0} else {floor(x)}}))
    # MEAN <- sapply(Mass.S, function(x) signif(mean(x),3))
    # SD <- sapply(Mass.S, function(x) signif(sd(x),2))
    
    # bar length attribution
    BW <- MEAN/max(MEAN)*2.8
    BW[BW < 0.006 & BW != 0] <- 0.006

    # save data for further processing
    write(paste0(names(Mass.S), ", ", MEAN, "$\\pm$", SD, ", ", round(BW,digits=2)),
          file = paste0("Charts/2017.10.20 Charts (TiKZ)/mass",sys,mat,".txt"))
    
    ##### UNSIMPLIFIED FLOWS #####
    
    ### CALCULATE FLOWS BETWEEN ALL COMPARTMENTS
    Mean.Flow <- matrix(NA,1,4, dimnames = list(NULL, c("Comp", "Dest", "Mean", "SD")))
    for(comp in Names){
      for(dest in names(TC.Norm[[sys]][[mat]][[comp]])){
        Flow[[sys]][[mat]][[comp]][[dest]] <- TC.Norm[[sys]][[mat]][[comp]][[dest]]*Mass[comp,]
        # append new flow to previous flows
        Mean.Flow <- rbind(Mean.Flow, c(comp, dest, mean(Flow[[sys]][[mat]][[comp]][[dest]]), sd(Flow[[sys]][[mat]][[comp]][[dest]])))
      }      
    }
    
    ### INPUT
    inp <- sapply(Input[[sys]][[mat]], function(x){if(is.null(x)){ rep(0,SIM) } else { x } })
    Mean.Input <- apply(inp,2,mean)
    SD.Input <- apply(inp,2,sd)
    To.Append <- cbind("Input", names(Mean.Input), Mean.Input, SD.Input)
    colnames(To.Append) <- c("Comp", "Dest", "Mean", "SD")
    # remove first line that contains only NA values
    Mean.Flow <- Mean.Flow[-1,]
    # append input to flows
    Mean.Flow <- rbind(To.Append, Mean.Flow)
    
    # export flows
    for(comp in c("Transport", "FibreProduction", "NonTextileManufacturing",
                  "TextileManufacturing", "Packaging", "BuildingConstruction",
                  "Automotive", "EEE", "Agriculture", "Other", "Apparel",
                  "HouseholdTextiles", "TechnicalTextiles")){
      if(!any(comp == Mean.Flow[,"Comp"] & "Export" == Mean.Flow[,"Dest"])){
        Mean.Flow <- rbind(Mean.Flow, c(comp, "Export", 0, 0))
      }
    }
    
    # ouflows
    Mean.Flow <- rbind(Mean.Flow,
                       c("MaterialReuse", "Output",
                         mean(Mass["MaterialReuse",]), sd(Mass["MaterialReuse",])))
    Mean.Flow <- rbind(Mean.Flow,
                       c("TextileReuse", "Output",
                         mean(Mass["TextileReuse",]), sd(Mass["TextileReuse",])))
    Mean.Flow <- rbind(Mean.Flow,
                       c("PartReuse", "Output",
                         mean(Mass["PartReuse",]), sd(Mass["PartReuse",])))
    Mean.Flow <- rbind(Mean.Flow,
                       c("WWTP", "Output",
                         mean(Mass["WWTP",]), sd(Mass["WWTP",])))
    
    # create flow names
    Flownames <- vector("numeric",nrow(Mean.Flow))
    for(i in 1:nrow(Mean.Flow)){
      Flownames[i] <- paste0(Mean.Flow[i,"Comp"], "2", Mean.Flow[i,"Dest"])
    }

    # format adequately
    MEAN <- as.numeric(Mean.Flow[,"Mean"])
    SD <- as.numeric(Mean.Flow[,"SD"])
    MEAN <- round(MEAN,-sapply(log(SD,10), function(x){ if(is.na(x)){NA} else if(!is.finite(x)){0} else {floor(x)}}))
    names(MEAN) <- Flownames
    
    # line width attribution
    if(sys == "EU"){
     
      LW <- sapply(MEAN,
                   function(x){
                     if(x >= 5000){ 
                       2.5
                     } else if(x >= 1000){ 2
                     } else if(x >= 500 ){ 1.5
                     } else if(x >= 100 ){ 1
                     } else if(x >= 10  ){ 0.5
                     } else if(x == 0   ){ 0
                     } else              { 0.25
                     }
                   })

    } else {

      LW <- sapply(MEAN,
                   function(x){
                     if(         x >= 500){ 2.5
                     } else if(x >= 100){ 2
                     } else if(x >= 50 ){ 1.5
                     } else if(x >= 10 ){ 1
                     } else if(x >= 1  ){ 0.5
                     } else if(x == 0  ){ 0
                     } else             { 0.25
                     }
                   })

    }

    # save data
    write(paste0(Flownames, ", ", MEAN, "$\\pm$", SD, ", ", LW, "mm"),
          file = paste0("Charts/2017.10.20 Advanced Charts (TikZ)/flow",sys,mat,".txt"))
    
    ##### SIMPLIFIED FLOWS #####
    Flow.S <- sapply(c("InToProd", "InToMan", "InToPack", "InToAuto", "InToEEE", "InToAppHH","InToTT",
                       "ProdToMan", "ProdToWC", "ProdToExp", "ManToPack", "ManToBC", "ManToAuto",
                       "ManToEEE", "ManToAgri", "ManToOther", "ManToApparelHH", "ManToTechText",
                       "ManToWC", "ManToExp", "PackToWC", "BCToWC", "AutoToWC", "EEEToWC", "AgriToWC",
                       "OtherToWC", "ApparelHHToWC", "TechTextToWC", "PackToMixedC", "EEEToMixedC",
                       "OtherToMixedC", "ApparelHHToMixedC", "TechTextToMixedC", "AutoToExp", "EEEToExp", "TechTextToExp",
                       "WCToRec", "WCToLf", "WCToInc", "WCToReuse", "WCToExp", "MixedCToLf", "MixedCToInc",
                       "RecToReuse", "RecToLf", "RecToInc", "RecToExp", "IncToElim", "OtherToWWTP"), function(x) NULL)
    # Input in production
    Flow.S[["InToProd"]] <- ( inp[,"PrimaryProduction"] +
                                inp[,"RecyclateRepelletizing"] )
    
    # Import flows
    Flow.S[["InToMan"]] <- ( inp[,"FibreProduction"] +
                                  inp[,"TextileManufacturing"] +
                                  inp[,"NonTextileManufacturing"] +
                                  inp[,"Transport"] )
    Flow.S[["InToPack"]]  <- inp[,"Packaging"]
    Flow.S[["InToAuto"]]  <- inp[,"Automotive"]
    Flow.S[["InToEEE"]]   <- inp[,"EEE"]
    Flow.S[["InToAppHH"]] <- ( inp[,"Apparel"] + inp[,"HouseholdTextiles"] )
    Flow.S[["InToTT"]]    <- inp[,"TechnicalTextiles"]

    # Flows from production
    Flow.S[["ProdToMan"]] <- ( Flow[[sys]][[mat]][["PrimaryProduction"]][["Transport"]] +
                                 Flow[[sys]][[mat]][["RecyclateRepelletizing"]][["Transport"]] )
    Flow.S[["ProdToWC"]] <- ( Flow[[sys]][[mat]][["PrimaryProduction"]][["PCCollection"]] +
                                Flow[[sys]][[mat]][["RecyclateRepelletizing"]][["PCCollection"]] )
    if(!is.null(Flow[[sys]][[mat]][["Transport"]][["Export"]])){
      Flow.S[["ProdToExp"]] <- Flow[[sys]][[mat]][["Transport"]][["Export"]]
    } else { Flow.S[["ProdToExp"]] <- rep(0,SIM) }
    
    # Flows from manufacturing
    Flow.S[["ManToPack"]]  <- Flow[[sys]][[mat]][["NonTextileManufacturing"]][["Packaging"]]
    Flow.S[["ManToBC"]]    <- Flow[[sys]][[mat]][["NonTextileManufacturing"]][["BuildingConstruction"]]
    Flow.S[["ManToAuto"]]  <- Flow[[sys]][[mat]][["NonTextileManufacturing"]][["Automotive"]]
    Flow.S[["ManToEEE"]]   <- Flow[[sys]][[mat]][["NonTextileManufacturing"]][["EEE"]]
    Flow.S[["ManToAgri"]]  <- Flow[[sys]][[mat]][["NonTextileManufacturing"]][["Agriculture"]]
    Flow.S[["ManToOther"]] <- Flow[[sys]][[mat]][["NonTextileManufacturing"]][["Other"]]
    Flow.S[["ManToApparelHH"]] <- ( Flow[[sys]][[mat]][["TextileManufacturing"]][["Apparel"]] +
                                      Flow[[sys]][[mat]][["TextileManufacturing"]][["HouseholdTextiles"]] )
    Flow.S[["ManToTechText"]]  <-  Flow[[sys]][[mat]][["TextileManufacturing"]][["TechnicalTextiles"]]

    Flow.S[["ManToWC"]]    <- ( Flow[[sys]][[mat]][["FibreProduction"]][["PCCollection"]] +
                                 Flow[[sys]][[mat]][["TextileManufacturing"]][["PCCollection"]] +
                                 Flow[[sys]][[mat]][["NonTextileManufacturing"]][["PCCollection"]] )
    
    Flow.S[["ManToExp"]] <- ( 
      if(!is.null(Flow[[sys]][[mat]][["NonTextileManufacturing"]][["Export"]])){
        Flow[[sys]][[mat]][["NonTextileManufacturing"]][["Export"]]} else {rep(0,SIM)} +
        if(!is.null(Flow[[sys]][[mat]][["TextileManufacturing"]][["Export"]])){
          Flow[[sys]][[mat]][["TextileManufacturing"]][["Export"]]} else {rep(0,SIM)} +
        if(!is.null(Flow[[sys]][[mat]][["FibreProduction"]][["Export"]])){
          Flow[[sys]][[mat]][["FibreProduction"]][["Export"]]} else {rep(0,SIM)} +
        if(!is.null(Flow[[sys]][[mat]][["Automotive"]][["Export"]])){
          Flow[[sys]][[mat]][["Automotive"]][["Export"]]} else {rep(0,SIM)}) 
    
    # Flows from consumption to separate waste collection
    Flow.S[["PackToWC"]] <- ( Flow[[sys]][[mat]][["ConsumerFilms"]][["PackagingCollection"]] +
                               Flow[[sys]][[mat]][["ConsumerBags"]][["PackagingCollection"]] + 
                               Flow[[sys]][[mat]][["ConsumerBottles"]][["PackagingCollection"]] + 
                               Flow[[sys]][[mat]][["ConsumerOther"]][["PackagingCollection"]] + 
                               Flow[[sys]][[mat]][["NonConsumerBags"]][["PackagingCollection"]] + 
                               Flow[[sys]][[mat]][["NonConsumerOther"]][["PackagingCollection"]] + 
                               Flow[[sys]][[mat]][["OtherNonConsumerFilms"]][["PackagingCollection"]] + 
                               Flow[[sys]][[mat]][["BuildingPackagingFilms"]][["CDIncinerableCollection"]] + 
                               Flow[[sys]][[mat]][["AgriculturalPackagingFilms"]][["AgricultureCollection"]] + 
                               Flow[[sys]][[mat]][["AgriculturalPackagingBottles"]][["AgricultureCollection"]] )
    
    Flow.S[["BCToWC"]] <- ( Flow[[sys]][[mat]][["PipesDucts"]][["CDCollection"]] +
                             Flow[[sys]][[mat]][["Insulation"]][["CDCollection"]] + 
                             Flow[[sys]][[mat]][["WallFloorCoverings"]][["CDCollection"]] + 
                             Flow[[sys]][[mat]][["WindowsProfilesFittedFurniture"]][["CDCollection"]] + 
                             Flow[[sys]][[mat]][["Lining"]][["CDCollection"]] )
    
    Flow.S[["AutoToWC"]] <- Flow[[sys]][[mat]][["AutomotivePC"]][["ELVCollection"]]
    
    Flow.S[["EEEToWC"]] <- Flow[[sys]][[mat]][["EEEPC"]][["WEEECollection"]]
    
    Flow.S[["AgriToWC"]] <- ( Flow[[sys]][[mat]][["AgriculturalFilms"]][["AgricultureCollection"]] +
                               Flow[[sys]][[mat]][["AgriculturalPipes"]][["AgricultureCollection"]] + 
                               Flow[[sys]][[mat]][["AgriculturalOther"]][["AgricultureCollection"]] )
    
    Flow.S[["OtherToWC"]] <- Flow[[sys]][[mat]][["FabricCoatings"]][["TextileCollection"]]
    
    Flow.S[["ApparelHHToWC"]] <- ( Flow[[sys]][[mat]][["ApparelPC"]][["TextileCollection"]] + 
                                    Flow[[sys]][[mat]][["HouseholdTextilesPC"]][["TextileCollection"]])
    
    Flow.S[["TechTextToWC"]] <- ( Flow[[sys]][[mat]][["TechnicalHouseholdTextiles"]][["TextileCollection"]] + 
                                   Flow[[sys]][[mat]][["TechnicalClothing"]][["TextileCollection"]] + 
                                   Flow[[sys]][[mat]][["Agrotextiles"]][["AgricultureCollection"]] + 
                                   Flow[[sys]][[mat]][["MobilityTextiles"]][["ELVTextilesCollection"]] + 
                                   Flow[[sys]][[mat]][["Geotextiles"]][["CDIncinerableCollection"]] + 
                                   Flow[[sys]][[mat]][["BuildingTextiles"]][["CDIncinerableCollection"]] )
    
    # Flows from consumption to mixed collection
    Flow.S[["PackToMixedC"]] <- ( Flow[[sys]][[mat]][["ConsumerFilms"]][["MixedCollection"]] +
                                   Flow[[sys]][[mat]][["ConsumerBags"]][["MixedCollection"]] + 
                                   Flow[[sys]][[mat]][["ConsumerBottles"]][["MixedCollection"]] + 
                                   Flow[[sys]][[mat]][["ConsumerOther"]][["MixedCollection"]] +
                                   Flow[[sys]][[mat]][["OtherNonConsumerFilms"]][["MixedCollection"]] +
                                   Flow[[sys]][[mat]][["NonConsumerBags"]][["MixedCollection"]] + 
                                   Flow[[sys]][[mat]][["NonConsumerOther"]][["MixedCollection"]] )
    
    Flow.S[["EEEToMixedC"]] <- Flow[[sys]][[mat]][["EEEPC"]][["MixedCollection"]]
    
    Flow.S[["OtherToMixedC"]] <- ( Flow[[sys]][[mat]][["Household"]][["MixedCollection"]] +
                                    Flow[[sys]][[mat]][["Furniture"]][["MixedCollection"]] + 
                                    Flow[[sys]][[mat]][["FabricCoatings"]][["MixedCollection"]] + 
                                    Flow[[sys]][[mat]][["OtherOther"]][["MixedCollection"]] )
    
    Flow.S[["ApparelHHToMixedC"]] <- ( Flow[[sys]][[mat]][["ApparelPC"]][["MixedCollection"]] + 
                                        Flow[[sys]][[mat]][["HouseholdTextilesPC"]][["MixedCollection"]] )
    
    Flow.S[["TechTextToMixedC"]] <- ( Flow[[sys]][[mat]][["TechnicalHouseholdTextiles"]][["MixedCollection"]] + 
                                       Flow[[sys]][[mat]][["TechnicalClothing"]][["MixedCollection"]] +
                                       Flow[[sys]][[mat]][["HygieneMedicalTextiles"]][["MixedCollection"]] + 
                                       Flow[[sys]][[mat]][["OtherTechnicalTextiles"]][["MixedCollection"]] )
    
    if(!is.null(Flow[[sys]][[mat]][["AutomotivePC"]][["Export"]])){
      Flow.S[["AutoToExp"]] <- Flow[[sys]][[mat]][["AutomotivePC"]][["Export"]]
    } else { Flow.S[["AutoToExp"]] <- rep(0,SIM) }
    
    if(!is.null(Flow[[sys]][[mat]][["EEEPC"]][["Export"]])){
      Flow.S[["EEEToExp"]] <- Flow[[sys]][[mat]][["EEEPC"]][["Export"]]
    } else { Flow.S[["EEEToExp"]] <- rep(0,SIM) }
    
    if(!is.null(Flow[[sys]][[mat]][["MobilityTextiles"]][["Export"]])){
      Flow.S[["TechTextToExp"]] <- Flow[[sys]][[mat]][["MobilityTextiles"]][["Export"]]
    } else { Flow.S[["EEEToExp"]] <- rep(0,SIM) }

    Flow.S[["OtherToWWTP"]] <- Flow[[sys]][[mat]][["Cosmetics"]][["WWTP"]]
    
    # flows from mixed collection
    Flow.S[["MixedCToInc"]] <- Flow[[sys]][[mat]][["MixedCollection"]][["Incineration"]]
    
    Flow.S[["MixedCToLf"]] <- Flow[[sys]][[mat]][["MixedCollection"]][["Landfill"]]
    
    # flows from separate collection
    Flow.S[["WCToRec"]] <- ( Flow[[sys]][[mat]][["PackagingCollection"]][["PackagingRecycling"]] +
                              Flow[[sys]][[mat]][["CDCollection"]][["CDRecycling"]] + 
                              Flow[[sys]][[mat]][["AgricultureCollection"]][["AgricultureRecycling"]] + 
                              Flow[[sys]][[mat]][["ELVCollection"]][["LargePartRecycling"]] + 
                              Flow[[sys]][[mat]][["ELVCollection"]][["ASRRecycling"]] + 
                              Flow[[sys]][[mat]][["WEEECollection"]][["WEEPRecycling"]] )
    
    Flow.S[["WCToLf"]] <- ( Flow[[sys]][[mat]][["PackagingCollection"]][["Landfill"]] +
                             Flow[[sys]][[mat]][["CDCollection"]][["Landfill"]] + 
                             Flow[[sys]][[mat]][["CDIncinerableCollection"]][["Landfill"]] +
                             Flow[[sys]][[mat]][["AgricultureCollection"]][["Landfill"]] + 
                             Flow[[sys]][[mat]][["ELVTextilesCollection"]][["Landfill"]] +
                             Flow[[sys]][[mat]][["WEEECollection"]][["Landfill"]] + 
                             Flow[[sys]][[mat]][["TextileCollection"]][["Landfill"]] +
                             Flow[[sys]][[mat]][["PCCollection"]][["Landfill"]] )
    
    Flow.S[["WCToInc"]] <- ( Flow[[sys]][[mat]][["PackagingCollection"]][["Incineration"]] +
                               Flow[[sys]][[mat]][["CDCollection"]][["Incineration"]] + 
                               Flow[[sys]][[mat]][["CDIncinerableCollection"]][["Incineration"]] +
                               Flow[[sys]][[mat]][["AgricultureCollection"]][["Incineration"]] + 
                               Flow[[sys]][[mat]][["ELVTextilesCollection"]][["Incineration"]] +
                               Flow[[sys]][[mat]][["WEEECollection"]][["Incineration"]] + 
                               Flow[[sys]][[mat]][["TextileCollection"]][["Incineration"]] +
                               Flow[[sys]][[mat]][["PCCollection"]][["Incineration"]] )
    
    Flow.S[["WCToReuse"]] <- ( Flow[[sys]][[mat]][["TextileCollection"]][["TextileReuse"]] +
                                Flow[[sys]][[mat]][["TextileCollection"]][["MaterialReuse"]] +
                                Flow[[sys]][[mat]][["PCCollection"]][["MaterialReuse"]] )
    
    Flow.S[["WCToExp"]] <- ( Flow[[sys]][[mat]][["PackagingCollection"]][["Export"]] +
                               Flow[[sys]][[mat]][["TextileCollection"]][["Export"]] )
    
    # flows from recycling
    Flow.S[["RecToReuse"]] <- ( Flow[[sys]][[mat]][["PackagingRecycling"]][["MaterialReuse"]] +
                                 Flow[[sys]][[mat]][["CDRecycling"]][["MaterialReuse"]] + 
                                 Flow[[sys]][[mat]][["AgricultureRecycling"]][["MaterialReuse"]] + 
                                 Flow[[sys]][[mat]][["LargePartRecycling"]][["MaterialReuse"]] + 
                                 Flow[[sys]][[mat]][["LargePartRecycling"]][["PartReuse"]] + 
                                 Flow[[sys]][[mat]][["ASRRecycling"]][["MaterialReuse"]] + 
                                 Flow[[sys]][[mat]][["WEEPRecycling"]][["MaterialReuse"]] )
    
    Flow.S[["RecToLf"]] <- ( Flow[[sys]][[mat]][["PackagingRecycling"]][["Landfill"]] +
                              Flow[[sys]][[mat]][["CDRecycling"]][["Landfill"]] + 
                              Flow[[sys]][[mat]][["AgricultureRecycling"]][["Landfill"]] + 
                              Flow[[sys]][[mat]][["LargePartRecycling"]][["Landfill"]] + 
                              Flow[[sys]][[mat]][["ASRRecycling"]][["Landfill"]] + 
                              Flow[[sys]][[mat]][["WEEPRecycling"]][["Landfill"]] )
    
    Flow.S[["RecToInc"]] <- ( Flow[[sys]][[mat]][["PackagingRecycling"]][["Incineration"]] +
                               Flow[[sys]][[mat]][["CDRecycling"]][["Incineration"]] + 
                               Flow[[sys]][[mat]][["AgricultureRecycling"]][["Incineration"]] + 
                               Flow[[sys]][[mat]][["LargePartRecycling"]][["Incineration"]] + 
                               Flow[[sys]][[mat]][["ASRRecycling"]][["Incineration"]] + 
                               Flow[[sys]][[mat]][["WEEPRecycling"]][["Incineration"]] )
    
    Flow.S[["RecToExp"]] <- ( Flow[[sys]][[mat]][["PackagingRecycling"]][["Export"]] )
      
    Flow.S[["IncToElim"]] <- Flow[[sys]][[mat]][["Incineration"]][["Elimination"]]
    
    # Format SD and MEAN appropriately
    MEAN <- sapply(Flow.S, mean)
   
    SD <- sapply(Flow.S, function(x) signif(sd(x),1))
    MEAN <- round(MEAN,-sapply(log(SD,10), function(x){ if(is.na(x)){NA} else if(!is.finite(x)){0} else {floor(x)}}))
    # MEAN <- sapply(Flow.S, function(x) signif(mean(x),3))
    # SD <- sapply(Flow.S, function(x) signif(sd(x),2))
    
    # line width attribution
    if(sys == "EU"){
      
      #print(c(sys, mat))
     
      print(MEAN)
      print(class(MEAN))
      lw<-import.test(MEAN)
      MEAN <- na.omit(MEAN)
      LW <- sapply(MEAN, 
                   function(x){
                     if(       x >= 5000){ 
                       2.5
                     } else if(x >= 1000){ 2
                     } else if(x >= 500 ){ 1.5
                     } else if(x >= 100 ){ 1
                     } else if(x >= 10  ){ 0.5
                     } else if(x == 0   ){ 0
                     } else              { 0.25
                     }
                   })
      
    }  else {
      MEAN <- na.omit(MEAN)
      LW <- sapply(MEAN,
                   function(x){
                     if(         x >= 500){ 2.5
                       } else if(x >= 100){ 2
                       } else if(x >= 50 ){ 1.5
                       } else if(x >= 10 ){ 1
                       } else if(x >= 1  ){ 0.5
                       } else if(x == 0  ){ 0
                       } else             { 0.25
                       }
                     })
      
    }
    
    # save data
    write(paste0(names(Flow.S), ", ", MEAN, "$\\pm$", SD, ", ", LW, "mm"),
          file = paste0("Charts/2020.12.09 Charts (TiKZ)/flow",sys,mat,".txt"))
  }
}

