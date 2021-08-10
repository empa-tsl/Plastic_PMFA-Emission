# Functions for the PMFA module
# Version 4
# Date of last modification: 28.06.2017

# =================================================================================================

# symmetric triangular distribution with percentage (default being 50%)
rtriang.perc <- function(mmode, perc = 0.5, N = SIM, linf = -Inf, lsup = Inf, is.tc = T)
{
  require(mc2d) # for rtrunc
  
  if(is.na(mmode)){
    return(rep(NA,N))
  }
  
  if(mmode == 0){
    return(rep(0,N))
  }
  
  if(is.tc){
    if(mmode == 1){
      return(rep(1,N))
    }
  }
  
  stopifnot(perc > 0,
            is.numeric(mmode),
            is.numeric(perc),
            is.numeric(N),
            is.numeric(linf),
            is.numeric(lsup),
            length(mmode) == 1,
            length(perc) == 1,
            length(N) == 1,
            length(linf) == 1,
            length(lsup) == 1,
            linf < lsup,
            linf <= mmode,
            lsup >= mmode)
  
  mmin <- mmode - abs(mmode*perc)
  mmax <- mmode + abs(mmode*perc)
  
  return(rtrunc(distr = "rtriang", n = N,
                linf = linf, lsup = lsup,
                min = mmin, mode = mmode, max = mmax))
}

# =================================================================================================

# trapezoidal distribution with percentage (default being 50%)
rtrapez.perc <- function(values, perc = c(0.5,0.5), N = SIM, linf = -Inf, lsup = Inf)
{
  require(mc2d) # for rtrunc
  require(trapezoid)
  
  if(any(is.na(values))){
    return(rep(NA,N))
  }
  
  stopifnot(all(perc[!is.na(perc)] > 0),
            is.numeric(values),
            is.numeric(perc),
            is.numeric(N),
            is.numeric(linf),
            is.numeric(lsup),
            length(values) == 2,
            length(perc) == 2,
            length(N) == 1,
            length(linf) == 1,
            length(lsup) == 1,
            linf < lsup,
            all(linf <= values),
            all(lsup >= values),
            all(is.finite(values)),
            all(is.finite(perc[values > 0])))
  
  # sort data
  perc <- perc[order(values)]
  values <- values[order(values)]
  
  if(is.na(perc[1]) & values[1] == 0){
    mmin <- 0
  } else {
    mmin <- values[1] - abs(values[1]*perc[1])  
  }
  
  mmax <- values[2] + abs(values[2]*perc[2])
  
  return(rtrunc(distr = "rtrapezoid", n = N,
                linf = linf, lsup = lsup,
                min = mmin, max = mmax,
                mode1 = values[1], mode2 = values[2]))
}

# =================================================================================================

# function that creates uniform distributions between vector numbers, with triangle at the borders
rmore <- function(values,
                  perc = NULL,
                  max = NULL,
                  min = NULL,
                  N = SIM,
                  linf = -Inf,
                  lsup = Inf){
  
  stopifnot(any(!is.null(max), !is.null(min), !is.null(perc)))
  
  if(!is.null(max) | !is.null(min)){
    stopifnot(!is.null(max),
              !is.null(min),
              !any(values > max),
              !any(values < min))
  }
  
  # Fill in the matrix. If there is only one endpoint, NOEC stays the same. If  there are
  # 2 endpoints, a uniform distribution is produced. If  there are more than 2 endpoints, a step
  # distribution is produced. One line is for one species.
  require(trapezoid)
  require(mc2d)
  
  # calculate the minimum and maximum of the distribution we are looking for
  if(!is.null(perc)){
    dist.min <- values[which.min(values)]*(1-perc[which.min(values)])
    dist.max <- values[which.max(values)]*(1+perc[which.max(values)])
  } else {
    dist.min <- min
    dist.max <- max
  }
  
  # remove all values which exceed the truncation limits
  perc   <- perc[  values <= lsup]
  values <- values[values <= lsup]
  perc   <- perc[  values >= linf]
  values <- values[values >= linf]
  
  # store sorted values
  sort.values <- sort(values)
  # store unique values
  uni.values <- unique(sort.values)
  # store frequency of unique values
  freq.uni.values <- table(sort.values)
  # smallest value
  val.min <- sort.values[1]
  # largest value
  val.max <- sort.values[length(sort.values)]
  
  
  # calculate the heights between each segment
  height.values <- rep(NA, length(uni.values))
  for(i in 2:(length(uni.values)-1)){
    height.values[i] <- max( freq.uni.values[i]*N/(uni.values[i+1]-uni.values[i]),
                             freq.uni.values[i]*N/(uni.values[i]-uni.values[i-1]) )
  }
  # first
  height.values[1] <- freq.uni.values[1]*N/(uni.values[2]-uni.values[1])
  # last
  height.values[length(height.values)] <-
    freq.uni.values[length(height.values)]*N/( uni.values[length(height.values)]-
                                                 uni.values[length(height.values)-1] )
  
  
  ### LEFT TRIANGLE ###############################################################################
  # if the lowest value equals the truncation limit, do nothing, as everything would be truncated otherwise
  if(val.min == linf){
    
    left <- NULL
    
    # # correct N for the portion of the distribution which will be missing
    # N <- ceiling(N*(1+1/length(uni.values)))
    # 
    # # recalculate heights with new N
    # height.values <- rep(NA, length(uni.values))
    # for(i in 2:(length(uni.values)-1)){
    #   height.values[i] <- max( freq.uni.values[i]*N/(uni.values[i+1]-uni.values[i]),
    #                            freq.uni.values[i]*N/(uni.values[i]-uni.values[i-1]) )
    # }
    # # last
    # height.values[length(height.values)] <-
    #   freq.uni.values[length(height.values)]*N/( uni.values[length(height.values)]-
    #                                                uni.values[length(height.values)-1] )
    # 
    # # skip the rest and continue to right triangle and inbetween distributions
    
    
  # otherwise, create left triangle
  } else {
    
    if((length(unique(sort.values)) < length(sort.values)) &
       (val.min == sort.values[2])){ 
      
      # how many values are identical
      n <- length(which(sort.values == val.min))
      # first value that is not identical to the previous one
      val.minP1 <- sort.values[n+1]
      # Height of the first value that is not identical to the previous ones
      h <- height.values[1]
      # Area of the triangular distribution, the first on the left-hand side
      A <- (val.min - dist.min)*h/2
      
      # Triangular distribution on the left-hand side
      # if the smallest value (and distribution max) is smaller than the truncation limit, do nothing
      if(val.min <= linf){
        left <- NULL
        
      } else {
        left <- rtrunc("rtriang",
                       n = A,
                       min = dist.min,
                       mode = val.min,
                       max = val.min,
                       lsup = lsup,
                       linf = linf)
      }
      
      # else if unique value
    } else {
      
      # Area of the triangular distribution, the first on the left-hand side
      A <- N*(val.min-dist.min)/(2*(sort.values[2]-val.min))
      
      # if the smallest value (and distribution max) is smaller than the truncation limit, do nothing
      if(val.min <= linf){
        left <- NULL
        
      } else {
        
        left <- rtrunc("rtriang",
                       n = A,
                       min = dist.min,
                       mode = val.min,
                       max = val.min,
                       lsup = lsup,
                       linf = linf)
      }
    }
  }
  
  
  ### RIGHT TRIANGLE ##############################################################################
  # if the highest value equals the truncation limit, do nothing, as everything would be truncated otherwise
  if(val.max == lsup){
    
    right <- NULL
    
    # # correct N for the portion of the distribution which will be missing
    # N <- ceiling(N*(1+1/length(sort.values)))
    # 
    # # recalculate heights with new N
    # height.values <- rep(NA, length(uni.values))
    # for(i in 2:(length(uni.values)-1)){
    #   height.values[i] <- max( freq.uni.values[i]*N/(uni.values[i+1]-uni.values[i]),
    #                            freq.uni.values[i]*N/(uni.values[i]-uni.values[i-1]) )
    # }
    # 
    # # skip the rest and continue to right triangle and inbetween distributions
    
    
  # otherwise, create right triangle
  } else {
    
    if((length(unique(sort.values)) < length(sort.values)) &
       (val.max == sort.values[length(sort.values)-1])){ 
      
      # how many values are identical
      n <- length(which(sort.values == val.max))
      
      # first endpoint from the right that is not identical to the later one
      val.maxM1 <- sort.values[length(sort.values)-n]
      # Height of the last endpoint that is not similar to the next ones
      h <- height.values[length(height.values)]
      # Area of the triangular distribution, the last on the right-hand side
      A <- (dist.max - val.max)*h /2
      
      # Triangular distribution on the right-hand side
      # if the largest value is larger than the truncation limit, do nothing
      if(dist.max > lsup){
        right <- NULL
        
      } else if(lsup == val.max){
        right <- NULL
        
        
      } else {
        right <- rtrunc("rtriang",
                        n = A,
                        min = val.max,
                        mode = val.max,
                        max = dist.max,
                        lsup = lsup,
                        linf = linf)
      }
      
      # else if unique value
    } else {
      
      # Area of the triangular distribution, the first on the left-hand side
      A <- N*(dist.max-val.max)/(2*(val.max-sort.values[length(sort.values)-1]))
      
      right <- rtrunc("rtriang",
                      n = A, 
                      min = sort.values[length(sort.values)], 
                      mode = sort.values[length(sort.values)],
                      max = dist.max,
                      lsup = lsup,
                      linf = linf)
    }
  }  
  
  ### INBETWEEN STUFF #############################################################################
  
  # Create a matrix in which each line is a uniform distribution. We need it from mode 2 to mode n-1
  mid <- list()
  
  for(i in 1:(length(uni.values)-1)){
    
    if(height.values[i] == 1 & height.values[i+1] == 1){
      # Calculate the uniform distributions in between
      
      mid[[i]] <- rtrunc("runif",
                         n = N,
                         min = uni.values[i],
                         max = uni.values[i+1],
                         lsup = lsup,
                         linf = linf)
      
      
    } else if(height.values[i] < height.values[i+1]){
      
      # height of shape
      h1 <- height.values[i]
      h2 <- height.values[i+1]
      # extreme edge of triangle
      min.trunc.distr <- (h2*uni.values[i] - h1*uni.values[i+1])/(h2 - h1)
      # area of the triangle
      A <- ((uni.values[i+1] - min.trunc.distr)*h2 - ( uni.values[i] - min.trunc.distr )*h1)/2
      
      mid[[i]] <- rtrunc("rtriang",
                         n = A,
                         min = min.trunc.distr,
                         mode = uni.values[i+1],
                         max = uni.values[i+1],
                         lsup = lsup,
                         linf = max(uni.values[i],linf))
      
      
      
    } else if(height.values[i] > height.values[i+1]){
      
      # height of shape
      h1 <- height.values[i]
      h2 <- height.values[i+1]
      # extreme edge of triangle
      max.trunc.distr <- (h1*uni.values[i+1]-h2*uni.values[i])/(h1-h2)
      # area of the triangle
      A <- ((max.trunc.distr - uni.values[i])*h1 - (max.trunc.distr - uni.values[i+1])*h2)/2
      
      mid[[i]] <- rtrunc("rtriang",
                         n = A,
                         min = uni.values[i],
                         mode = uni.values[i],
                         max = max.trunc.distr,
                         lsup = min(uni.values[i+1],lsup),
                         linf = linf)
      
      
      
    } else if(height.values[i] == height.values[i+1]){
      mid[[i]] <- rtrunc("runif",
                         n = N*freq.uni.values[i],
                         min = uni.values[i],
                         max = uni.values[i+1],
                         lsup = lsup,
                         linf = linf)
    }
    
    
  }
  
  
  
  
  
  # Combine left handside, middle and right handside distributions.
  # Each distribution has the same weight (length(mid)=nb EP * N).
  step_distr <- c(left,do.call("c", mid),right)
  
  return(sample(step_distr, N))
  
}


# =================================================================================================

# wrapper function that chooses whether to use rtriang.perc, rtrapez.perc or rmore
rwrap <- function(values, perc, N = SIM, linf = -Inf, lsup = Inf){
  
  stopifnot(length(values) == length(perc))
  
  # for very small values, problems with precision arise, and R cannot compute any sort of
  # distribution. To overcome the problem, the values are instead sampled.
  if(all(values < 10^-7)){
    return(sample(values, SIM, replace = T))
  }
  
  # for one parameter value, do a triangular distribution
  if(length(values) == 1){
    return(rtriang.perc(mmode = values, perc = perc, N = N, linf = linf, lsup = lsup))
    
    # for two parameter values, do a trapezoidal distribution
  } else if(length(values) == 2){
    return(rtrapez.perc(values = values, perc = perc, N = N, linf = linf, lsup = lsup))
    
    # for more parameter values, do a step distribution
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
import.TC <- function(Coeff, Materials){
  
  TC <- sapply(Materials, function(x) NULL)
  
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
      
      if (is.na(dest) || is.na(comp)) {
        next
      }
      
      # find the data corresponding to destination
      data <- Coeff[Coeff[,"From"] == comp & Coeff[,"To"] == dest,
                    c("From", "To", "Material", "Data", "Spread")]
      
      data <- na.omit(data)
      
      # in case there is just one, need to format as a matrix, to avoid errors
      if(is.null(dim(data))){ 
        dim(data) <- c(1,5)
        colnames(data) <- c("From", "To", "Material", "Data", "Spread")
      }
      
      if(any(is.na(data[,c("From", "To", "Material")]))){
        stop(paste("The flow from", comp, "to", dest, "is ill-defined."))
      }
      
      # loop over materials, find corresponding data, attribute it
      for(mat in Materials){
        
        # find mat data in data
        datapoints <- which(data[,"Material"] %in% c("any", mat))
        
        if(any(is.na(data[datapoints,"Spread"]))){
          warning(paste0("Unexisting spread for the flow from ", comp, " to ", dest, "."))
        }
        
        # for undefined data, fill with NA values
        if(any(is.na(data[datapoints,"Data"]))){
          TC[[mat]][[comp]][[dest]] <- rep(NA,SIM)
          next
        }
        
        # for compartments where the flow is defined as the rest, fill with NA data and move on to next iteration
        if(any(data[datapoints,"Data"] == "rest")){
          TC[[mat]][[comp]][[dest]] <- rep("rest",SIM)
          next
        }
        
        # else create a distribution from all these values
        TC[[mat]][[comp]][[dest]] <- rwrap(values = as.numeric(data[datapoints, "Data"]), 
                                           perc = as.numeric(data[datapoints, "Spread"]),
                                           N = SIM, 
                                           linf = 0,
                                           lsup = 1)
      }
    }
  }
  
  return(TC)
  
}

# function to calculate "rest" TC
calc.rest.TC <- function(TC){
  
  # for compartments where the flow is defined as the rest, take the rest
  for(mat in 1:length(TC)){
    for(comp in 1:length(TC[[mat]])){
      
      # find the destination compartment that is a 'rest' flow
      restcomp <- which(sapply(TC[[mat]][[comp]], function(x) any(x == "rest")))
      
      # if there is more than one rest flow, give error
      if(length(restcomp) > 1){
        stop("There can't be more than one compartment with a 'rest' flow.")
        
        # if there is no rest flow, go next
      } else if(length(restcomp) == 0){
        next
      }
      
      # if one flow is undefined, do nothing
      if(any(sapply(TC[[mat]][[comp]], function(x) any(is.na(x))))){
        next
      }
      
      # find the other flows from the compartment
      otherflows <- TC[[mat]][[comp]]
      otherflows[[restcomp]] <- NULL
      
      # in case more than one other flow, sum them up
      otherflows <- apply(do.call(cbind, otherflows), 1, sum)
      if(any(otherflows > 1)){
        message( "WARNING: Sum of outflows from ",
                 names(TC[[mat]])[comp],
                 " > 1 (",
                 round(length(which(otherflows > 1))/length(otherflows)*100, digits = 2),
                 "% of iterations). TC to ",
                 names(TC[[mat]][[comp]])[restcomp],
                 " contains artificial zeroes.")
      }
      
      # subtract from one, tadaaa!
      TC[[mat]][[comp]][[restcomp]] <- ifelse(otherflows > 1, 0, 1 - otherflows)
      
      # redefine other flows to make sure the normalization does not introduce negative values
      if(any(otherflows > 1)){
        
        # redefine the sum of the otherflows to match the rest flow
        redefotherflows <- 1 - TC[[mat]][[comp]][[restcomp]]
        
        for(dest in names(TC[[mat]][[comp]])){
          
          # skip rest flow
          if(dest == restcomp){ next }
          
          # else renormalize it
          TC[[mat]][[comp]][[dest]] <- TC[[mat]][[comp]][[dest]]*redefotherflows/otherflows
          
        }
        
      }
    }
  }
  
  return(TC)
  
}

# function to import data stored in Excel for input
import.input <- function(data, Materials){
  
  # create empty lists
  Input <- sapply(Materials, function(x) NULL)
  for(mat in Materials){
    Input[[mat]] <- sapply(Names, function(x) NULL)
  }
  
  
  # take appropriate data for the system
  data <- data[, c("Compartment", "Material", "Data..kt.", "Spread")]
  
  # take only the data for which there is data and uncertainty provided
  data <- data[apply(data, 1, function(x) all(!(is.na(x) | x == 0))),]
  
  # find compartments for which there is a flow
  compartment <- unique(data[,"Compartment"])
  
  # loop over compartments with an inflow
  for(comp in compartment){
    # loop over materials
    for(mat in Materials){
      
      # find data corresponding to the compartment and material
      datapoints <- which(data[,"Compartment"] == comp & data[,"Material"] == mat)
      
      # create distribution
      if(all(as.numeric(data[datapoints, "Data..kt."]) >= 0)){
        Input[[mat]][[comp]] <- rwrap(values = as.numeric(data[datapoints, "Data..kt."]), 
                                      perc = as.numeric(data[datapoints, "Spread"]),
                                      N = SIM,
                                      linf = 0)
      } else if(all(as.numeric(data[datapoints, "Data..kt."]) <= 0)){
        Input[[mat]][[comp]] <- rwrap(values = as.numeric(data[datapoints, "Data..kt."]), 
                                      perc = as.numeric(data[datapoints, "Spread"]),
                                      N = SIM,
                                      lsup = 0)
      } else {
        Input[[mat]][[comp]] <- rwrap(values = as.numeric(data[datapoints, "Data..kt."]), 
                                      perc = as.numeric(data[datapoints, "Spread"]),
                                      N = SIM)
        message("-------- WARNING: There are positive and negative input values for the same compartment (",mat,",",comp,"), consider revising the input data.")
      } 
      
    }
  }
  return(Input)
  
}
