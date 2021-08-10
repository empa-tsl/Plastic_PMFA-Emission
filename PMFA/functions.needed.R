# Functions for the PMFA module
# Version 4
# Date of last modification: 28.06.2017

# =================================================================================================

# symmetric triangular distribution with percentage (default being 50%)
rtriang.perc <- function(mmode, perc = 0.5, N = SIM, linf = -Inf, lsup = Inf)
{
  require(mc2d) # for rtrunc
  
  if(is.na(mmode)){
    return(rep(NA,N))
  }
  
  if(mmode == 0){
    return(rep(0,N))
  }
  
  if(mmode == 1){
    return(rep(1,N))
  }
  
  stopifnot(perc >= 0)
  
  mmin <- mmode - abs(mmode*perc)
  mmax <- mmode + abs(mmode*perc)
  
  return(rtrunc(distr = "rtriang", n = N,
                linf = linf, lsup = lsup,
                min = mmin, mode = mmode, max = mmax))
}

# =================================================================================================

# trapezoidal distribution with percentage (default being 50%)
rtrapez.perc <- function(values, perc, N = SIM, linf = -Inf, lsup = Inf)
{
  require(mc2d) # for rtrunc
  require(trapezoid)
  
  stopifnot(length(values) == 2, perc > 0)
  if(any(is.na(values))){
    return(rep(NA,N))
  }
  
  # sort data
  perc <- perc[order(values)]
  values <- values[order(values)]
  
  mmin <- values[1] - abs(values[1]*perc[1])
  mmax <- values[2] + abs(values[2]*perc[2])
  
  return(rtrunc(distr = "rtrapezoid", n = N,
                linf = linf, lsup = lsup,
                min = mmin, max = mmax,
                mode1 = values[1], mode2 = values[2]))
}

# =================================================================================================

# function that creates uniform distributions between vector numbers, with triangle at the borders
rmore <- function(values, perc, N = SIM, linf = -Inf, lsup = Inf){
  require(mc2d) # for rtrunc
  require(trapezoid)
  
  blocks <- sapply(1:(length(values)-1), function(x) NULL )
  Nseq <- ceiling(N/(length(values)-1))
  
  # check for incompatible values
  stopifnot(all(values >= linf), all(values <= lsup),
            all(is.finite(values)))
  
  # sort values and uncertainty
  perc <- perc[order(values)]
  values <- values[order(values)]
  
  # create a uniform distribution for all the middle points
  for(i in 2:(length(values)-2)){
    blocks[[i]] <- rtrunc(distr = "runif", n = Nseq,
                          linf = linf, lsup = lsup,
                          min = values[i], max = values[i+1])
  }
  
  ### lower end of the distribution
  # if the two first values are different, proceed as expected
  if(values[2] != values[1]){
    # Nseq needs to be adapted in order to have a 'continuous' distribution
    Nalt <- Nseq*(1+perc[1]*values[1]/(2*(values[2]-values[1])))
    blocks[[1]] <- rtrunc(distr = "rtrapezoid", n = Nalt,
                          linf = linf, lsup = lsup,
                          mode1 = values[1], mode2 = values[2],
                          min = abs(1-perc[1])*values[1], max = values[2])
    
  } else {
    if(values[1] != 0){
      warning("This version of the function cannot calculate the lower tail of the distribution if the two smallest values are equal and non-zero. See code for details and update the function if necessary.")
    }
    blocks[[1]] <- rtrunc(distr = "runif", n = Nseq,
                          linf = linf, lsup = lsup,
                          min = values[1], max = values[2])
  }
  
  ### upper end of the distribution
  # if the two last values are equal
  if(values[length(values)] == values[length(values)-1]){
    warning("This version of the function cannot calculate the upper tail of the distribution if the two biggest values are equal. See code for details and update the function if necessary.")
    blocks[[length(blocks)]] <- rtrunc(distr = "runif", n = Nseq,
                                       linf = linf, lsup = lsup,
                                       min = values[length(values)-1],
                                       max = abs(1+perc[length(values)])*values[length(values)])
  } else {
    # Nseq needs to be adapted in order to have a 'continuous' distribution
    Nalt <- Nseq*(1+perc[length(values)]*values[length(values)]/(2*(values[length(values)]-values[length(values)-1])))
    blocks[[length(blocks)]] <- rtrunc(distr = "rtrapezoid", n = Nalt,
                                       linf = linf, lsup = lsup,
                                       min = values[length(values)-1], mode1 = values[length(values)-1],
                                       mode2 = values[length(values)],
                                       max = abs(1+perc[length(values)])*values[length(values)])
  }
  
  # aggregate all the data together and return the required amount of points
  distr <- unlist(blocks)
  return(sample(distr, size = N, replace = F))
}

# =================================================================================================

# wrapper function that chooses whether to use rtriang.perc, rtrapez.perc or rmore
rwrap <- function(values, perc, N = SIM, linf = -Inf, lsup = Inf){
  # print(c("perc: ", perc))
  stopifnot(length(values) == length(perc))
  
  if(length(values) == 1){
    return(rtriang.perc(mmode = values, perc = perc, N = N, linf = linf, lsup = lsup))
  } else if(length(values) == 2){
    return(rtrapez.perc(values = values, perc = perc, N = N, linf = linf, lsup = lsup))
  } else {
    return(rmore(values = values, perc = perc, N = N, linf = linf, lsup = lsup))
  }
}

# =================================================================================================

# introduce function that normalizes outflows
normalize <- function(Distr){
  
  # 1. Test the input
  if(!is.list(Distr)){
    stop("Distr should be a list of lists.")
  }
  
  # 2. Normalization step: make sure that all the flows going out of a compartment sum up to 1
  Distr.Norm <- Distr
  
  for(j in 1:length(Distr)){
    
    # if there are no outflows, leave as it is
    if(is.null(Distr[[j]])){
      next
      
    } else {
      # sum the flows that leave a compartment
      thesum <- apply(sapply(Distr[[j]], cbind), 1, sum)
      
      if(all(thesum == 0)){ next }
      
      for(i in 1:length(Distr[[j]])){
        Distr.Norm[[j]][[i]] <- Distr[[j]][[i]]/thesum
      }
    }
  }
  
  message(paste(format(Sys.time(), "%H:%M:%S"),"Normalization complete.")) 
  
  return(Distr.Norm)
}

# =================================================================================================

# Monte-Carlo simulation
solve.MC <- function(TC.Distr, inp.Distr, Names, N){
  
  # prepare output
  Mass <- matrix(NA, length(Names), N, dimnames = list(Names, NULL))
  
  # Monte-Carlo iterations
  for(k in 1:N){
    
    # create empty TC matrix
    themat <- matrix(0, length(Names), length(Names), dimnames = list(Names, Names))
    # create empty input vector
    theinput <- sapply(Names, function(x) 0)
    
    for(comp in Names){
      # prepare k-th matrix for equation solving
      if(!is.null(TC.Distr[[comp]])){
        for(dest in 1:length(TC.Distr[[comp]])){
          themat[names(TC.Distr[[comp]])[dest],comp] <- TC.Distr[[comp]][[dest]][k]
        }
      }
      # prepare k-th input for equation solving
      if(!is.null(inp.Distr[[comp]])){
        theinput[comp] <- inp.Distr[[comp]][k]
      }
    }
    
    # transform the matrix
    themat <- -themat
    diag(themat) <- 1
    
    # solve equation
    Mass[,k] <- solve(themat, theinput)
  }
  
  message(paste(format(Sys.time(), "%H:%M:%S"),"Simulation complete."))
  
  return(Mass)
}

# =================================================================================================

# function to import data stored in Excel for transfer coefficients
import.TC <- function(Coeff, Systems, Materials){
  
  TC <- sapply(Systems, function (x) NULL)
  for(sys in Systems){
    TC[[sys]] <- sapply(Materials, function(x) NULL)
  }
 
  # find which compartments from which something flows
  fromcomp <- unique(Coeff[,"From"])
  
  # loop over compartments
  for(comp in fromcomp){
    
    # find destination compartments from this compartment
    tocomp <- unique(Coeff[Coeff[,"From"] == comp,"To"])
    # if there is no destination compartment, go to next iteration
    if(length(tocomp) == 0){ next }
    
    # else loop over destination compartments
    for(dest in tocomp){
      # find the data corresponding to destination
      data <- Coeff[Coeff[,"From"] == comp & Coeff[,"To"] == dest, c("From", "To", "Material", "System", "Data", "SpreadEU", "SpreadCH")]
      if(is.null(dim(data))){ # in case there is just one, need to format as a matrix, to avoid errors
        dim(data) <- c(1,7)
        colnames(data) <- c("From", "To", "Material", "System", "Data", "SpreadEU", "SpreadCH")
      }
      
      # loop over systems and materials, find corresponding data, attribute it
      for(sys in Systems){
        for(mat in Materials){
          
          if(sys=="EU"& mat=="PUR"&comp=="ConsumerBottles"&dest=="MixedCollection"){
            print(c("hello", datapoints))
            browser()
          }
          # print(data[,"Material"] %in% c("any",mat))
          # print(data[,"Material"])
          # print(data[,"System"])
          # print(sys)
          # print(c("any",sys))
          # print(data[,"System"] %in% c("any", sys))
          # print(which(data[,"Material"] %in% c("any", mat) & data[,"System"] %in% c("any", sys)))
          # print(data[,"Material"] %in% c("any", mat) & data[,"System"] %in% c("any", sys))
          # find sys and mat data in data
          datapoints <- which(data[,"Material"] %in% c("any", mat) & data[,"System"] %in% c("any", sys))
          #print(sys)
          #print(c("datapoints: ", datapoints))
          #if(sys=="EU" &  datapoints=="1"){
            #print("hello")}
          # for undefined data, fill with NA values
          #browser()
          if(any(is.na(data[datapoints,"Data"]))){
            TC[[sys]][[mat]][[comp]][[dest]] <- rep(NA,SIM)
            next
          }
          
          # for compartments where the flow is defined as the rest, fill with NA data and move on to next iteration
          if(any(data[datapoints,"Data"] == "rest")){
            TC[[sys]][[mat]][[comp]][[dest]] <- rep("rest",SIM)
            next
          }
          # print(c(sys, mat, comp, dest))
          
          # Muyu's Modification
          # if(any(is.na(as.numeric(data[datapoints, paste0("Spread",sys)])))){
          #   next()
          # }
          
          # else create a distribution from all these values
          TC[[sys]][[mat]][[comp]][[dest]] <- rwrap(values = as.numeric(data[datapoints, "Data"]), 
                                                    perc = as.numeric(data[datapoints, paste0("Spread",sys)]),
                                                    N = SIM, 
                                                    linf = 0,
                                                    lsup = 1)
        }
      }
    }
  }
  
  return(TC)
  
}

# function to calculate "rest" TC
calc.rest.TC <- function(TC){
  
  # for compartments where the flow is defined as the rest, take the rest
  for(sys in 1:length(TC)){
    for(mat in 1:length(TC[[sys]])){
      for(comp in 1:length(TC[[sys]][[mat]])){
        
        # find the destination compartment that is a 'rest' flow
        restcomp <- which(sapply(TC[[sys]][[mat]][[comp]], function(x) any(x == "rest")))
        
        # if there is more than one rest flow, give error
        if(length(restcomp) > 1){
          stop("There can't be more than one compartment with a 'rest' flow.")
          
          # if there is no rest flow, go next
        } else if(length(restcomp) == 0){
          next
        }
        
        # browser()
        # find the other flows from the compartment
        otherflows <- TC[[sys]][[mat]][[comp]]
        otherflows[[restcomp]] <- NULL
        
        if (length(otherflows) == 0) {
          next()
        }
        
        # in case more than one other flow, sum them up
        otherflows <- apply(do.call(cbind, otherflows), 1, sum)
        if(any(otherflows > 1)){
          message( "-------- WARNING: The sum of the defined outflows from ",
                   names(TC[[sys]][[mat]])[comp],
                   " is not always <= 1 (",
                   round(length(which(otherflows > 1))/length(otherflows)*100, digits = 2),
                   "% of the occurrences). As a consequence, the TC to ",
                   names(TC[[sys]][[mat]][[comp]])[restcomp],
                   " contains artificial zeroes.")
        }
        
        # subtract from one, tadaaa!
        TC[[sys]][[mat]][[comp]][[restcomp]] <- ifelse(otherflows > 1, 0, 1 - otherflows)
        
        # redefine other flows to make sure the normalization does not introduce negative values
        if(any(otherflows > 1)){
          
          # redefine the sum of the otherflows to match the rest flow
          redefotherflows <- 1 - TC[[sys]][[mat]][[comp]][[restcomp]]
          
          for(dest in names(TC[[sys]][[mat]][[comp]])){
            
            # skip rest flow
            if(dest == restcomp){ next }
            
            # else renormalize it
            TC[[sys]][[mat]][[comp]][[dest]] <- TC[[sys]][[mat]][[comp]][[dest]]*redefotherflows/otherflows
            
          }
          
        }
      }
    }
  }
  
  return(TC)
  
}

# function to import data stored in Excel for input
import.input <- function(Inp.EU, Inp.CH, Systems, Materials){
  
  # create empty lists
  Input <- sapply(Systems, function (x) NULL)
  for(sys in Systems){
    Input[[sys]] <- sapply(Materials, function(x) NULL)
    
    for(mat in Materials){
      Input[[sys]][[mat]] <- sapply(Names, function(x) NULL)
    }
  }
  
  # loop over systems
  for(sys in Systems){
    
    # take appropriate data for the system
    if(sys == "EU"){ data <- Inp.EU } else { data <- Inp.CH }
    #data <- Inp.EU
    data <- data[, c("Compartment", "Material", "Data..kt.", "Spread")]
    #browser()
    # take only the data for which there is data and uncertainty provided
    data <- data[apply(data, 1, function(x) all(!(is.na(x) | x == 0))),]
    
    # find compartments for which there is a flow
    compartment <- unique(data[,"Compartment"])
    
    # loop over compartments with an inflow
    for(comp in compartment){
      # loop over material
      #if(comp == "FibreProduction")
        #browser()
      
      for(mat in Materials){
        # if(comp == "TextileManufacturing" & mat == "PC")
          #browser()
        # print(c("comp:",comp))
        # print(c("mat", mat))
        # find data corresponding to the compartment and material
        datapoints <- which(data[,"Compartment"] == comp & data[,"Material"] == mat)
        
        # Muyu's Modification
        if(all(is.na(as.numeric(data[datapoints, "Spread"])))){
          next()
        }
        
        # create distribution
        if(all(as.numeric(data[datapoints, "Data..kt."]) >= 0)){
          Input[[sys]][[mat]][[comp]] <- rwrap(values = as.numeric(data[datapoints, "Data..kt."]), 
                                               perc = as.numeric(data[datapoints, "Spread"]),
                                               N = SIM,
                                               linf = 0)
        } else if(all(as.numeric(data[datapoints, "Data..kt."]) <= 0)){
          Input[[sys]][[mat]][[comp]] <- rwrap(values = as.numeric(data[datapoints, "Data..kt."]), 
                                               perc = as.numeric(data[datapoints, "Spread"]),
                                               N = SIM,
                                               lsup = 0)
        } else {
          Input[[sys]][[mat]][[comp]] <- rwrap(values = as.numeric(data[datapoints, "Data..kt."]), 
                                               perc = as.numeric(data[datapoints, "Spread"]),
                                               N = SIM)
          message("-------- WARNING: There are positive and negative input values for the same compartment (",sys,",",mat,",",comp,"), consider revising the input data.")
        } 
        
      }
    }
  }
  
  return(Input)
  
}
