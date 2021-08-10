# =================================================================================================
# Scientific notation
# sci.not <- function(MEAN, SD, type = "LaTeX", sep = NULL){
#   
#   MEAN.sn <- round(MEAN,
#                    - sapply( log(SD,10),
#                              function(x){ 
#                                if(is.na(x)){
#                                  NA 
#                                } else if(!is.finite(x)){
#                                  0
#                                } else {
#                                  floor(x)
#                                }
#                              } )
#   )
#   
#   
#   
#   
#   if(type == "LaTeX"){
#     return(paste0(MEAN.sn, "$\\pm$", signif(SD,1)))
#   } else {
#     if(is.null(sep)){
#       return(paste0(MEAN.sn, " ? ", signif(SD,1)))
#     } else {
#       return(paste0(MEAN.sn, sep, signif(SD,1)))
#     }
#   }
#   
#   
# }

# =================================================================================================

# # for testing
# setwd("~/Projects/2018 PMFA Emissions/Programming/2017.11 Module 2 - Trial 2/")
# load("Input/InputReady_Mod1-2.Rdata")
# TC.Distr <- TC.Norm[["EU"]][["PET"]]
# comp <- "PipesDucts"
# verbose <- TRUE

# function for finding the final compartments
find.fc <- function(comp, # the name of the compartment to investigate
                    mass = NULL, # the mass needed to multiply the TCs, if NULL, don't multiply
                    stop.at = NULL, # vector of the names of the compartments at which the calculation needs to stop
                    TC.Distr, # list containing the TCs
                    verbose = T, # logical: whether to comment on each calculation step
                    loop.break = 50) # for safety if there is a problem, number of iterations at which to stop
{
  # need a condition for the while loop, set to TRUE to start
  cond <- T
  
  # need an equivalent list to TC.Distr for storing FC values
  FC.Distr <- TC.Distr[[comp]]
  
  # for counting loops
  i <- 1
  while(any(cond)){
    
    if(verbose){ message(paste0("\n\nIteration #",i)) }
    
    # create a condition vector that will be modified within the for loop
    cond <- sapply(names(FC.Distr), function(x) T)
    
    # loop over destinations
    for(dest in names(FC.Distr)){
      
      # if no outflow from destination compartment, skip as it is a sink
      if(is.null(TC.Distr[[dest]])){
        # change condition to FALSE, so that if everything is FALSE, loop stops
        cond[dest] <- F
        
        if(verbose){ message(paste(comp, "->", dest, ":", "Skipped")) }
        
        # if compartment is in the "stop.at" compartments, stop there
      } else if(dest %in% stop.at){
        # change condition to FALSE, so that if everything is FALSE, loop stops
        cond[dest] <- F
        
        if(verbose){ message(paste(comp, "->", dest, ":", "Ignored")) }
        
        # else calculate
      } else {
        
        # else if the TC from compartment "dest" is non-null, find the compartments
        for(dest2 in names(TC.Distr[[dest]])){
          
          # if dest2 already exists as destination in the list, add the contribution
          if(!is.null(FC.Distr[[dest2]])){
            FC.Distr[[dest2]] <- ( FC.Distr[[dest2]] + 
                                     FC.Distr[[dest]]*TC.Distr[[dest]][[dest2]] )
            if(verbose){ message(paste0(comp, " -> (", dest, ") -> ", dest2, " : ", "Added to preexisting")) }
            
          } else {
            # else add the list element
            FC.Distr[[dest2]] <- FC.Distr[[dest]]*TC.Distr[[dest]][[dest2]]
            
            if(verbose){ message(paste0(comp, " -> (", dest, ") -> ", dest2, " : ", "Added")) }
          }
          
        }
        
        # and remove the first, otherwise it will be added again
        FC.Distr[[dest]] <- NULL
      }
    }
    
    # increase loop iteration number
    i <- i+1
    
    # test for precision
    if(verbose){ 
      total <- apply(do.call("cbind", FC.Distr), 1, sum)
      if(all(abs(total - 1) < 10^-9)){
        message("\nSum of out-TCs is 1 (precision of at least 10^-9)")
      } else {
        message("\nSum of out-TCs is ", mean(total))
      }
    }
    
    if(i>loop.break){
      cond <- F
      message("\n!!! Calculation interrupted artificially after ",loop.break, " iterations.\n... Check if there may be a closed loop in the defined flows, or increase the limit of 'loop.break'.")
    }
  }
  
  # test for precision
  total <- apply(do.call("cbind", FC.Distr), 1, sum)
  if(!all(abs(total - 1) < 10^-9)){
    warning("\n--------------- Sum of out-TCs is ", mean(total), " ---------------")
  }
  
  # multiply with mass
  if(!is.null(mass)){
    for(cc in names(FC.Distr)){
      FC.Distr[[cc]] <- FC.Distr[[cc]]*mass
    }
  }
  
  return(FC.Distr)
}


# function for aggregating the final compartments
agg.fc <- function(FC)
{
  # obtain all the names of the final compartments
  agg <- sapply(unique(unlist(lapply(FC, names))), function(x) NULL)
  
  # check if there are names for the list elements
  if(is.null(names(agg))){
    stop("The list elements need to have names, they can't be recognized otherwise.")
  }
  
  # loop over all existing final compartments
  for(fc in names(agg)){
    agg[[fc]] <- rep(0,SIM)
    
    # for each final compartment in FC, add in the right list element
    for(orig in 1:length(FC)){
      for(dest in names(FC[[orig]])){
        
        if(dest != fc){ next }
        agg[[fc]] <- agg[[fc]] + FC[[orig]][[fc]]
        
      }
    }
    
  }
  
  return(agg)
}

# function for aggregating the flows by compartment
agg.flows <- function(Flows, # list of flows
                      comps) # list of flows to aggregate
{
  if(any(duplicated(unlist(comps))))
  {
    stop("One compartment cannot be attributed to two aggregated elements.")
  }
  
  # do it for each aggregated compartment
  for(agg.comp in names(Flows)){
    
    # loop over translation list
    for(trsl in names(comps)){
      
      # if a compartment from the translation list corresponds to an element in the flow list
      if( any( comps[[trsl]] %in% names(Flows[[agg.comp]]) ) ){
        
        # find the compartments corresponding to the aggregated compartments from trsl
        add.these <- comps[[trsl]][ comps[[trsl]] %in% names(Flows[[agg.comp]]) ]
        
        if(length(add.these) == 1){
          # if there is only one compartment, change name separately to avoid errors
          Flows[[agg.comp]][[trsl]] <- Flows[[agg.comp]][[add.these]]
          
        } else {
          # calculate the sum and insert it
          Flows[[agg.comp]][[trsl]] <- apply(do.call("cbind",Flows[[agg.comp]])[,add.these],1,sum)
        } 
        
        # remove the rest
        for(tt in comps[[trsl]]){
          Flows[[agg.comp]][[tt]] <- NULL  
        }
        
      }
      
    }
    
  }
  
  return(Flows)
}

# function for calculating flows (multiplying TC list by mass of compartment)
mult.tc <- function(TC, mass)
{
  # loop over all destination compartments
  for(comp in names(TC)){
    TC[[comp]] <- TC[[comp]]*mass
    
  }
  
  return(TC)
  
}

# functions that return flows if it exists
null.flo <- function(Flows, comp, SIM){
  if(is.null(Flows[[comp]])){
    return(rep(0, SIM))
  } else {
    return(Flows[[comp]])
  }
}


##### MODIFIED VIOPLOT FUNCTION FOR COLOURS #######################################################
# adapted from https://stackoverflow.com/questions/14975853/how-can-i-create-violin-plot-in-different-colours
library(sm)
vioplot <- function(x,...,range=1.5,h=NULL,ylim=NULL,names=NULL, horizontal=FALSE,
                    col="magenta", border="black", lty=1, lwd=1, rectCol="black", colMed="white",
                    pchMed=19, at, add=FALSE, wex=1, las=1, xlab="", ylab="", main="",
                    drawRect=TRUE)
{
  # process multiple datas
  datas <- list(x,...)
  n <- length(datas)
  
  # if only one list is given, treat as only data
  if(n == 1 & class(x) == "list"){
    datas <- x
    n <- length(datas)
  }
  
  if(missing(at)) at <- 1:n
  
  # pass 1
  #
  # - calculate base range
  # - estimate density
  #
  
  # setup parameters for density estimation
  upper  <- vector(mode="numeric",length=n)
  lower  <- vector(mode="numeric",length=n)
  q1     <- vector(mode="numeric",length=n)
  q3     <- vector(mode="numeric",length=n)
  med    <- vector(mode="numeric",length=n)
  base   <- vector(mode="list",length=n)
  height <- vector(mode="list",length=n)
  baserange <- c(Inf,-Inf)
  
  # global args for sm.density function-call
  args <- list(display="none")
  
  if (!(is.null(h)))
    args <- c(args, h=h)
  
  for(i in 1:n) {
    data<-datas[[i]]
    
    # calculate plot parameters
    #   1- and 3-quantile, median, IQR, upper- and lower-adjacent
    data.min <- min(data)
    data.max <- max(data)
    q1[i]<-quantile(data,0.25)
    q3[i]<-quantile(data,0.75)
    med[i]<-median(data)
    iqd <- q3[i]-q1[i]
    upper[i] <- min( q3[i] + range*iqd, data.max )
    lower[i] <- max( q1[i] - range*iqd, data.min )
    
    #   strategy:
    #       xmin = min(lower, data.min))
    #       ymax = max(upper, data.max))
    #
    
    est.xlim <- c( min(lower[i], data.min), max(upper[i], data.max) )
    
    # estimate density curve
    smout <- do.call("sm.density", c( list(data, xlim=est.xlim), args ) )
    
    # calculate stretch factor
    #
    #  the plots density heights is defined in range 0.0 ... 0.5
    #  we scale maximum estimated point to 0.4 per data
    #
    hscale <- 0.4/max(smout$estimate) * wex
    
    # add density curve x,y pair to lists
    base[[i]]   <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    
    # calculate min,max base ranges
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1],t[1])
    baserange[2] <- max(baserange[2],t[2])
    
  }
  
  # pass 2
  #
  # - plot graphics
  
  # setup parameters for plot
  if(!add){
    xlim <- if(n==1)
      at + c(-.5, .5)
    else
      range(at) + min(diff(at))/2 * c(-1,1)
    
    if (is.null(ylim)) {
      ylim <- baserange
    }
  }
  if (is.null(names)) {
    label <- 1:n
  } else {
    label <- names
  }
  
  boxwidth <- 0.05 * wex
  
  # setup plot
  if(!add)
    plot.new()
  if(!horizontal) {
    if(!add){
      plot.window(xlim = xlim, ylim = ylim)
      axis(2, las = las)
      axis(1, at = at, label = label, las = las)
      title(main = main, xlab = xlab, ylab = ylab)
    }
    
    box()
    for(i in 1:n) {
      # plot left/right density curve
      polygon( c(at[i]-height[[i]], rev(at[i]+height[[i]])),
               c(base[[i]], rev(base[[i]])),
               col = col[i], border=border, lty=lty, lwd=lwd)
      
      if(drawRect){
        # plot IQR
        lines( at[c( i, i)], c(lower[i], upper[i]) ,lwd=lwd, lty=lty)
        
        # plot 50% KI box
        rect( at[i]-boxwidth/2, q1[i], at[i]+boxwidth/2, q3[i], col=rectCol)
        
        # plot median point
        points( at[i], med[i], pch=pchMed, col=colMed )
      }
    }
    
  }
  else {
    if(!add){
      plot.window(xlim = ylim, ylim = xlim)
      axis(1)
      axis(2,at = at, label=label, las = las)
      title(main = main, xlab = xlab, ylab = ylab)
    }
    
    box()
    for(i in 1:n) {
      # plot left/right density curve
      polygon( c(base[[i]], rev(base[[i]])),
               c(at[i]-height[[i]], rev(at[i]+height[[i]])),
               col = col[i %% length(col) + 1], border=border, lty=lty, lwd=lwd)
      
      if(drawRect){
        # plot IQR
        lines( c(lower[i], upper[i]), at[c(i,i)] ,lwd=lwd, lty=lty)
        
        # plot 50% KI box
        rect( q1[i], at[i]-boxwidth/2, q3[i], at[i]+boxwidth/2,  col=rectCol)
        
        # plot median point
        points( med[i], at[i], pch=pchMed, col=colMed )
      }
    }
  }
  invisible (list( upper=upper, lower=lower, median=med, q1=q1, q3=q3))
}
# error bar plotting function from http://monkeysuncle.stanford.edu/?p=485
error.bar <- function(x, y, upper, lower=upper, length=0.1, horiz=F,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  if(horiz){
    segments(y+upper, x, y-lower, x, ...)
  } else {
    segments(x,y+upper, x, y-lower, ...)
  }
}
