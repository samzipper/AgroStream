## interp.R
# This function is intended to take in two vectors, y (the variable you want to interpolate) and x (the
# variable interpolation is based on), and a value 'x.in'. The function will find the two values in x that 
# bookend the value x.in and linearly interpolate between them to produce an output variable 'y.out'.

# sample data for testing
#x <- seq(0,5,1)
#y <- c(1,2,4,8,16,32)

interp <- function(x,y,x.in){

  y.out <- numeric(length(x.in))
  
  for (i.x in x.in){
    ## First, check if x.in is equal (within a tolerance) to any value in x
    eps <- .Machine$double.eps^0.5 # tolerance parameter
    if (any(abs(x-i.x)<eps)){
      y.out <- y[which(abs(x-i.x)<eps)]
    } else {
      
      # make a list of indices the length of x
      i <- seq(1,length(x),1)
      
      # to find one end, find the index of the closest value in x to x.in which is less than x.in
      i.lt <- i[x==max(x[x < i.x])]
      
      # find the index of the closest value in x to x.in which is greater than x.in
      i.gt <- i[x==min(x[x > i.x])]
      
      # get corresponding values of y
      y.lt <- y[i.lt]
      y.gt <- y[i.gt]
      
      # interpolate a new value of y.out
      y.out[which(x.in==i.x)] <- y.lt+(y.gt-y.lt)*(x.in-x[i.lt])/(x[i.gt]-x[i.lt])
    }
    
  }
  
  return(y.out)
    
}

# testing
#interp(x,y,-1)
