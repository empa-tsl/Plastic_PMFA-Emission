# import data
load("D:\WORKSPACE\zipeng\Input\InputReady.Rdata")

library("xlsx")
NameDataBase <- as.matrix(read.xlsx("2018test2.xlsx", sheetName = "Names"))
rownames(NameDataBase) <- NameDataBase[,1]
NameDataBase <- NameDataBase[,2:3]

nmat <- length(Materials)
matcolors <- rainbow(nmat)

for(sys in Systems){
  
  pdf(file = paste0("Results/TC_Boxplot_",sys,".pdf"),
      height = 5,
      width = 6,
      pointsize = 10)
  par(mfrow = c(1,1), mar = c(3,3,3,1), mgp = c(1.8,0.5,0), xpd = F)
  
  for(comp in Names){
    
    # check whether there is data to plot for this compartment for any material
    do.next <- sapply(Materials, function(x) NA)
    for(mat in Materials){
      # check if the flow is missing, or if the flow is always 1
      if(is.null(TC.Norm[[sys]][[mat]][[comp]]) | length(TC.Norm[[sys]][[mat]][[comp]]) == 1){
        do.next[mat] <- T
      } else {
        do.next[mat] <- F
      }
    }
    # if no material has anything important, skip compartment
    if(!any(!do.next)){
      plot(1,1, type = "n", axes = F, xlab = "", ylab = "", main = comp)
      text(1,1, labels = "Unique or no outflow", cex = 2, font = 2, col = "darkred")
      next
    }
    
    # find number of destination compartments
    do.next <- sapply(Materials, function(x) NA)
    destnames <- sapply(Materials, function(x) NULL)
    for(mat in Materials){
      do.next[mat] <- length(TC.Norm[[sys]][[mat]][[comp]])
      destnames[[mat]] <- names(TC.Norm[[sys]][[mat]][[comp]])
    }
    ndest <- max(do.next)
    destnames <- unique(unlist(destnames))
    
    # create empty boxplot
    boxplot(x = matrix(0, 3, (ndest*(nmat+1)-1)), 
            axes = F,
            las = 2,
            ylim = c(0,1),
            ylab = "Transfer Coefficient",
            main = "",
            border = NA,
            col = NA)
    abline(h=seq(0,1,0.2), col = "gray80")
    axis(2, las = 2)
    mtext(comp, side = 3, line = 1, font = 2, cex = 1.2, adj = 0)
    # axis(1)
    box()
    legend("topright", Materials, ncol = 4, fill = matcolors, inset = c(0,-0.1), xpd = T,
           cex = 0.6, bty = "n")
    
    for(n in 1:ndest){
      
      polygon(x = c((n-1)*(nmat+1)+0.5, (n-1)*(nmat+1)+0.5,
                    n*(nmat+1)-0.5, n*(nmat+1)-0.5),
              y = c(-0.2, 1.2, 1.2, -0.2),
              border = NA,
              col = adjustcolor(rainbow(ndest)[n], alpha.f = 0.2))
      
      mtext(text = NameDataBase[destnames[n], "ShortLabel"],
            side = 1,
            line = 0.5,
            at = c(mean(c((n-1)*(nmat+1)+1, n*(nmat+1)))),
            cex = 0.5)
    }
    

    for(themat in 1:nmat){
      mat <- Materials[themat]  
      
      if(ndest == length(TC.Norm[[sys]][[mat]][[comp]])){
        boxplot(x = TC.Norm[[sys]][[mat]][[comp]],
                names = rep("", ndest),
                at = (themat + c(0:(ndest-1))*(nmat+1)),
                las = 2,
                ylim = c(0,1),
                xlab = "",
                ylab = "",
                main = "",
                range = 0,
                col = matcolors[themat],
                add = T,
                axes = F)
      } else {
        boxplot(x = TC.Norm[[sys]][[mat]][[comp]],
                names = rep("", ndest-1),
                at = (themat + c(0:(ndest-2))*(nmat+1)),
                las = 2,
                ylim = c(0,1),
                xlab = "",
                ylab = "",
                main = "",
                range = 0,
                col = matcolors[themat],
                add = T,
                axes = F)
      }
      
      
    }
  }
  
  dev.off()
  
}

