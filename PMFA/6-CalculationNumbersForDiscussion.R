# import input data
load("Input/InputReady.Rdata")
source("test.R")
# create empty vectors for flows
Flow <- sapply(Systems, function (x) NULL)
Flow[["EU"]] <- Flow[["CH"]] <- sapply(Materials, function(x) NULL)
for(mat in Materials){
  Flow[["EU"]][[mat]] <- Flow[["CH"]][[mat]] <- sapply(Names, function(x) NULL)
}

Masses <- array(NA, c(length(Systems),length(Materials),length(Names), SIM), dimnames = list(Systems, Materials, Names, NULL))
for(sys in Systems){
  for(mat in Materials){
    # import output data
    load(paste0("Results/ResultsMass_NoSimplification/OutputMass_",sys,"_",mat,".Rdata"))
    Masses[sys,mat,,] <- Mass
    
    for(comp in Names){
      for(dest in names(TC.Norm[[sys]][[mat]][[comp]])){
        Flow[[sys]][[mat]][[comp]][[dest]] <- TC.Norm[[sys]][[mat]][[comp]][[dest]]*Mass[comp,]
      }      
    }
    
    rm(Mass, comp, dest)
  }
}


SCW <- function(x){
  if(all(x == 0)){
    return(paste("0?0"))
  }
  MEAN <- mean(x)
  SD <- signif(sd(x),1)
  paste0(round(MEAN,-sapply(log(SD,10), function(y){ if(is.na(y)){NA} else if(!is.finite(y)){0} else {floor(y)}})),
        "?",SD)
}

IN0 <- function(x){
  if(is.null(x)){
    0
  } else {
    x
  }
}

############ ORIGIN OF INPUT ######################################################################
cat("--- ORIGIN OF INPUT ---\n\n")
for(mat in Materials){
  cat("Input analysis for", mat, "in the EU:\n")
  cat("Production:",SCW(Input[["EU"]][[mat]][["PrimaryProduction"]]), "\n")
  
  cat("Recycling:",SCW(Input[["EU"]][[mat]][["RecyclateRepelletizing"]]), "\n")
  
  cat("Preliminary products:",SCW(IN0(Input[["EU"]][[mat]][["Transport"]]) +
        IN0(Input[["EU"]][[mat]][["FibreProduction"]]) +
        IN0(Input[["EU"]][[mat]][["NonTextileManufacturing"]]) +
        IN0(Input[["EU"]][[mat]][["TextileManufacturing"]])), "\n")
  
  cat("Finished products:",
        SCW(IN0(Input[["EU"]][[mat]][["Packaging"]]) +
        IN0(Input[["EU"]][[mat]][["BuildingConstruction"]]) +
        IN0(Input[["EU"]][[mat]][["Agriculture"]]) +
        IN0(Input[["EU"]][[mat]][["Automotive"]])+
        IN0(Input[["EU"]][[mat]][["EEE"]]) +
        IN0(Input[["EU"]][[mat]][["Other"]]) +
        IN0(Input[["EU"]][[mat]][["Apparel"]])+
        IN0(Input[["EU"]][[mat]][["HouseholdTextiles"]]) +
        IN0(Input[["EU"]][[mat]][["TechnicalTextiles"]])), "\n")
  
  cat("Production is",
      SCW( IN0(Input[["EU"]][[mat]][["PrimaryProduction"]])/
             ( IN0(Input[["EU"]][[mat]][["PrimaryProduction"]]) +
                 IN0(Input[["EU"]][[mat]][["RecyclateRepelletizing"]]) +
                 IN0(Input[["EU"]][[mat]][["Transport"]]) +
                 IN0(Input[["EU"]][[mat]][["FibreProduction"]]) +
                 IN0(Input[["EU"]][[mat]][["NonTextileManufacturing"]]) +
                 IN0(Input[["EU"]][[mat]][["TextileManufacturing"]]) +
                 IN0(Input[["EU"]][[mat]][["Packaging"]]) +
                 IN0(Input[["EU"]][[mat]][["BuildingConstruction"]]) +
                 IN0(Input[["EU"]][[mat]][["Agriculture"]]) +
                 IN0(Input[["EU"]][[mat]][["Automotive"]])+
                 IN0(Input[["EU"]][[mat]][["EEE"]]) +
                 IN0(Input[["EU"]][[mat]][["Other"]]) +
                 IN0(Input[["EU"]][[mat]][["Apparel"]])+
                 IN0(Input[["EU"]][[mat]][["HouseholdTextiles"]]) +
                 IN0(Input[["EU"]][[mat]][["TechnicalTextiles"]]))*100 ),
      "% of the total input.")
  
  cat("\n")
}

cat("\n\n")

############ FRACTION OF TEXTILE PRODUCTS #########################################################
sys <- "EU"
cat("--- TEXTILE CONSUMPTION ---\n\n")
for(mat in c("PUR", "PA")){
  PC <- c(names(TC.Norm[[sys]][[mat]][["Packaging"]]),
          names(TC.Norm[[sys]][[mat]][["BuildingConstruction"]]),
          names(TC.Norm[[sys]][[mat]][["Agriculture"]]),
          names(TC.Norm[[sys]][[mat]][["Automotive"]]),
          names(TC.Norm[[sys]][[mat]][["EEE"]]),
          names(TC.Norm[[sys]][[mat]][["Other"]]),
          names(TC.Norm[[sys]][[mat]][["Apparel"]]),
          names(TC.Norm[[sys]][[mat]][["HouseholdTextiles"]]),
          names(TC.Norm[[sys]][[mat]][["TechnicalTextiles"]]))
  PC <- PC[!PC == "Export"]
  
  Text.PC <- c(names(TC.Norm[[sys]][[mat]][["Apparel"]]),
               names(TC.Norm[[sys]][[mat]][["HouseholdTextiles"]]),
               names(TC.Norm[[sys]][[mat]][["TechnicalTextiles"]]))
  Text.PC <- Text.PC[!Text.PC == "Export"]
  
  cat("Textile consumption share for", mat, "in the EU:",
      SCW(apply(Masses[sys,mat,Text.PC,],2,sum)/apply(Masses[sys,mat,PC,],2,sum)*100),
      "%\n")
}

cat("\n\n")

############ FRACTION OF WASTE MANAGEMENT OPTIONS #################################################
sys <- "EU"
cat("--- WASTE MANAGEMENT OPTIONS ---\n\n")
for(mat in Materials){
  cat("Share landfilled for", mat, "in the EU:",
      SCW(Masses[sys,mat,"Landfill",]/apply(Masses[sys,mat,c("Landfill", "Incineration", 
                                                            "MaterialReuse", "TextileReuse",
                                                            "PartReuse"),],2,sum)*100),
      "%\n")
  cat("Share incinerated for", mat, "in the EU:",
      SCW(Masses[sys,mat,"Incineration",]/apply(Masses[sys,mat,c("Landfill", "Incineration", 
                                                             "MaterialReuse", "TextileReuse",
                                                             "PartReuse"),],2,sum)*100),
      "%\n")
  cat("Share reused for", mat, "in the EU:",
      SCW(apply(Masses[sys,mat,c("MaterialReuse", "TextileReuse", "PartReuse"),],2,sum)/
            apply(Masses[sys,mat,c("Landfill", "Incineration", 
                                                             "MaterialReuse", "TextileReuse",
                                                             "PartReuse"),],2,sum)*100),
      "%\n\n")
}

cat("\n\n")

###### EXPORTED PRELIM PLASTIC ##########################
cat("\n--- EXPORTED PRELIM PLASTIC ---\n\n")
for(mat in Materials){
  cat("Amount of primary",
      mat,
      "exported:",
      SCW(IN0(TC.Norm[["EU"]][[mat]][["Transport"]][["Export"]]*100)),
      "%\n")
}
cat("\n\n")

############### PORTION COLLECTED SEPARATELY ######################################################

cat("\n--- COLLECTED SEPARATELY ---\n\n")
for(mat in Materials){
  cat(mat,
      "collected separately:",
      SCW(
        apply(Masses["EU", mat, c("PackagingCollection","CDCollection",
                            "CDIncinerableCollection","ELVCollection","ELVTextilesCollection",
                            "WEEECollection","AgricultureCollection","TextileCollection",
                            "PCCollection"),],2,sum) /
          apply(Masses["EU", mat, c("PackagingCollection","MixedCollection","CDCollection",
                              "CDIncinerableCollection","ELVCollection","ELVTextilesCollection",
                              "WEEECollection","AgricultureCollection","TextileCollection",
                              "PCCollection"),],2,sum) *
          100
        
      ),
      "%\n")
}
cat("\n\n")


########## PLASTIC BAGS CONSUMPTION ###############################################################

#cat("--- PLASTIC BAG CONSUMPTION ---\n\n")
#cat("Consumer plastic bags consumption: ", mean(apply(Masses["EU", , "ConsumerBags", ], 2, sum)), "?", sd(apply(Masses["EU", , "ConsumerBags", ], 2, sum)),
#    "\nNon-consumer plastic bags consumption: ", mean(apply(Masses["EU", , "NonConsumerBags", ], 2, sum)), "?", sd(apply(Masses["EU", , "NonConsumerBags", ], 2, sum)),
#    "\nNC and C plastic bags consumption: ", mean(apply(Masses["EU", , "ConsumerBags", ], 2, sum) + apply(Masses["EU", , "NonConsumerBags", ], 2, sum)), "?", sd(apply(Masses["EU", , "ConsumerBags", ], 2, sum) + apply(Masses["EU", , "NonConsumerBags", ], 2, sum)))
#cat("\n\n")


########## PP textiles PC #########################################################################


######### QUANTITY OF TEXTILES IMPORTED INTO CH ###################################################

######### GEOTEXTILES COMPOSITION ######################################


########### PVC WASTE GENERATED PER CAPITA ######################################
# EU28 population 2014 from
# Population change - Demographic balance and crude rates at national level [demo_gind]
# Eurostat
# retrieved 27.09.2017
# http://ec.europa.eu/eurostat/data/database?node_code=proj#

############### PVC product categories #########################

### POLYMER DEMAND #####################################################
# for(mat in Materials){
#   cat(mat,
#       " demand:",
#       round(mean(Masses["EU",mat, "Transport",]), digits = 0),
#       "?",
#       round(sd(Masses["EU",mat, "Transport",]), digits = 0),
#       "\n")
# }
# 
# for(mat in Materials){
#   cat(mat,
#       " demand:",
#       round(mean(Masses["EU",mat, "NonTextileManufacturing",]), digits = 0),
#       "?",
#       round(sd(Masses["EU",mat, "NonTextileManufacturing",]), digits = 0),
#       "\n")
# }


### ORIGIN OF TEXTILES ################################################

cat("Fraction of textiles coming from abroad:\n")
for(sys in Systems){
  for(mat in c("PUR", "PA")){
    
    cat(sys, "-", mat, "-", "Clothing:",
        SCW(Input[[sys]][[mat]][["Apparel"]] / ( 
          Flow[[sys]][[mat]][["TextileManufacturing"]][["Apparel"]] + 
            Input[[sys]][[mat]][["Apparel"]] ) ),
        "\n")
    
    cat(sys, "-", mat, "-", "HouseholdTextiles:",
        SCW(Input[[sys]][[mat]][["HouseholdTextiles"]] / ( 
          Flow[[sys]][[mat]][["TextileManufacturing"]][["HouseholdTextiles"]] + 
            Input[[sys]][[mat]][["HouseholdTextiles"]] ) ),
        "\n")
    
    cat(sys, "-", mat, "-", "TechnicalTextiles:",
        SCW(Input[[sys]][[mat]][["TechnicalTextiles"]] / ( 
          Flow[[sys]][[mat]][["TextileManufacturing"]][["TechnicalTextiles"]] + 
            Input[[sys]][[mat]][["TechnicalTextiles"]] ) ),
        "\n")
    
  }
}
cat("\n\n")

### ORIGIN OF AUTOMOTIVE PLASTICS ################################################

cat("Fraction of automotive plastics coming from abroad:\n")
for(sys in Systems){
  for(mat in Materials){
    
    
    if(is.null(Input[[sys]][[mat]][["Automotive"]])){
      next
    }
    
    if(all(Flow[[sys]][[mat]][["NonTextileManufacturing"]][["Automotive"]] + 
           Input[[sys]][[mat]][["Automotive"]] == 0)){
      next
    }
    
    cat(sys, "-", mat, ":",
        SCW(Input[[sys]][[mat]][["Automotive"]] / ( 
          Flow[[sys]][[mat]][["NonTextileManufacturing"]][["Automotive"]] + 
            Input[[sys]][[mat]][["Automotive"]] ) ),
        "\n")
   
  }
}
cat("\n\n")

### ORIGIN OF PACKAGING PLASTICS ################################################

cat("Fraction of automotive plastics coming from abroad:\n")
for(sys in Systems){
  for(mat in Materials){
    
    
    if(is.null(Input[[sys]][[mat]][["Packaging"]])){
      next
    }
    
    if(all(Flow[[sys]][[mat]][["NonTextileManufacturing"]][["Packaging"]] + 
           Input[[sys]][[mat]][["Packaging"]] == 0)){
      next
    }
    
    cat(sys, "-", mat, ":",
        SCW(Input[[sys]][[mat]][["Packaging"]] / ( 
          Flow[[sys]][[mat]][["NonTextileManufacturing"]][["Packaging"]] + 
            Input[[sys]][[mat]][["Packaging"]] ) ),
        "\n")
    
  }
}
cat("\n\n")

