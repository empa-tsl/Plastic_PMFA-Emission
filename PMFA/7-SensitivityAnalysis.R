# import input data
load("Input/InputReady.Rdata")

newSIM <- 100

for(sys in Systems){
  for(mat in Materials){
    # find the names of compartments and where they flow to
    temp <- sapply(TC.Norm[[sys]][[mat]], names)
    # create a vector with the names of the flows
    flownames <- "Zut"
    for(comp in names(temp)){
      flownames <- c(flownames, paste0(comp, "2", temp[[comp]]))
    }
    flownames <- flownames[-1]
    
    # create empty matrix to store all the Kendall correlation coefficients
    Cor.Mat <- matrix(data = NA,
                      nrow = length(Names)+length(unlist(temp)),
                      ncol = length(Names),
                      dimnames = list(c(Names, flownames), Names))
    
    # import output data
    load(paste0("Results/ResultsMass_NoSimplification/OutputMass_",sys,"_",mat,".Rdata"))
    # take only parts, because calculating everything would need days
    Mass <- Mass[,1:newSIM]
    
    for(RES in Names){
      for(comp in Names){
        
        # correlation with input
        if(!is.null(Input[[sys]][[mat]][[comp]])){
          Cor.Mat[comp, RES] <- cor(x = Mass[RES,],
                                    y = Input[[sys]][[mat]][[comp]][1:newSIM],
                                    use = "all.obs",
                                    method = "kendall") 
        }
        
        # correlation with TCs
        for(dest in names(TC.Norm[[sys]][[mat]][[comp]])){
          Cor.Mat[paste0(comp, "2", dest), RES] <- cor(x = Mass[RES,],
                                                       y = TC.Norm[[sys]][[mat]][[comp]][[dest]][1:newSIM],
                                                       use = "all.obs",
                                                       method = "kendall")
        }
      }      
      
    }
    
    save(Cor.Mat, file = paste0("Results/RData files/Kendall_", sys, "_", mat, ".RData"))
    
    
    {
      pdf(file = paste0("Results/Sensitivity/Sensitivity_Map_",sys,"_",mat,".pdf"),
          height = 20,
          width  = 15,
          pointsize = 10)
      par(mfrow = c(1,1), mar = c(12,20,12,20), mgp = c(3,1,0), xpd = F)
      
      # set up empty plot
      to.plot <- t(Cor.Mat[rev(dimnames(Cor.Mat)[[1]]),])
      to.plot <- to.plot[apply(to.plot, 1, function(x) !all(is.na(x))),
                         apply(to.plot, 2, function(x) !all(is.na(x)))]
      
      image(to.plot, axes = F, col = NA)
      
      abline(v=seq(0,1,length.out = dim(to.plot)[[1]]), col = "gray80", lty = 1)
      abline(h=seq(0,1,length.out = dim(to.plot)[[2]]), col = "gray80", lty = 1)
      
      # set up colors
      image(to.plot >= 0.8, axes = F, col = c(NA, "forestgreen"), add = T)
      image(to.plot >= 0.6 & to.plot < 0.8, axes = F, col = c(NA, "chartreuse4"), add = T)
      image(to.plot >= 0.4 & to.plot < 0.6, axes = F, col = c(NA, "chartreuse3"), add = T)
      image(to.plot >= 0.2 & to.plot < 0.4, axes = F, col = c(NA, "chartreuse1"), add = T)
      
      image(to.plot <= -0.8, axes = F, col = c(NA, "firebrick4"), add = T)
      image(to.plot <= -0.6 & to.plot > -0.8, axes = F, col = c(NA, "firebrick3"), add = T)
      image(to.plot <= -0.4 & to.plot > -0.6, axes = F, col = c(NA, "firebrick2"), add = T)
      image(to.plot <= -0.2 & to.plot > -0.4, axes = F, col = c(NA, "firebrick1"), add = T)
      
      # image(to.plot < 0.2, axes = F, col = c(NA, "white"), add = T)
      
      image(is.na(to.plot), axes = F, col = c(NA, "gray80"), add = T)
      
      axis(side = 1,
           at = seq(0,1,length.out = dim(to.plot)[[1]]),
           labels = dimnames(to.plot)[[1]], las = 2)
      
      axis(side = 3,
           at = seq(0,1,length.out = dim(to.plot)[[1]]),
           labels = dimnames(to.plot)[[1]], las = 2)
      
      axis(side = 2,
           at = seq(0,1,length.out = dim(to.plot)[[2]]),
           labels = dimnames(to.plot)[[2]], las = 2)
      
      axis(side = 4,
           at = seq(0,1,length.out = dim(to.plot)[[2]]),
           labels = dimnames(to.plot)[[2]], las = 2)
      
      box()

      dev.off() 
      
    }
    
    
    
    rm(Masses, Cor.Mat)
  }
}


