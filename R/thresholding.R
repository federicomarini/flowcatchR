

#' otsuThreshold
#' 
#' Determines the value for the threshold grey level according to the Otsu method 
#'  
#' @param input_image An Image object
#' @param nr_bit Number of bits to use for discretizing the levels
#' 
#' @return A numeric value
#'  
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
otsuThreshold <- function (input_image,nr_bit=16)  # use all channels?
{
  # image is read through readImage from EBImage
  # returns the level where the variance between classes is maximized
  ##STEPS NOT NEEDED!!!!    
  # transform into grey levels
  #  greyImg <- EBImage::channel(input_image,"grey")
  # the matrix gets stretched into a vec
  #  greyVec <- as.vector(greyImg)
  
  
  ## in order to treat properly the different channels, "step out like this"
  
  greyVec <- input_image
  totPixels <- length(greyVec)
  # and the values are rescaled
  #  greyLevels <- greyVec * 255
  #  greyLevels <- round(greyLevels)
  
  #  tableGrey <- tabulate(greyLevels)
  # tableGrey <- tableGrey + 0.0001
  
  #  n_i <- tableGrey
  #  p_i <- n_i/totPixels
  # careful, sum is not exactly 1...
  
  ## coding in 16 bit, slightly better
  greyLevels <- greyVec * (2^nr_bit -1) 
  
  #### modified!!
  greyLevels <- as.vector(greyLevels)
  
  
  greyLevels <- round(greyLevels)
  
  tableGrey <- tabulate(greyLevels)
  tableGrey <- tableGrey + 0.000001
  
  n_i <- tableGrey
  p_i <- n_i/totPixels
  # careful, sum is not exactly 1...
  
  ## defining functions for means and probabilities - what goes afterwards in the otsu formula to maximize
  ##
  mean_T <- function(p, L) #L=length(tableGrey) )
  {
    ind <- 1:L
    s <- ind * p[1:L]
    z <- sum(s)
    return(z)
  }
  ##
  mean_k <- function(p, k, L)
  {
    ind <- 1:k
    s <- ind * p[1:k]
    z <- sum(s)
    return(z)
  }
  ##
  w_k <- function(p, k)
  {
    s <- sum(p[1:k])
    return(s)
  }
  
  ## define the function to optimize
  sigma_B_k <- function(p, L, k)
  {
    ( mean_T(p,L) * w_k(p,k) - mean_k(p,k) )^2 / ( w_k(p,k) * (1 - w_k(p,k)) )
  }
  
  ## actually do the optimization (max required)
  o <- optimize(sigma_B_k, c(1,length(tableGrey)), maximum=TRUE, L=length(tableGrey), p=p_i )
  otsu_threshold <- o$maximum/(2^nr_bit-1) 
  
  cat("Calculated value with Otsu thresholding method...")
  cat(otsu_threshold)
  cat("\n")
  
  return(otsu_threshold)
  
}


################

#' kittlerillingerThreshold
#' 
#' Determines the value for the threshold grey level according to the Kittler-Illinger method 
#'   
#' @param input_image An Image object
#' @param nr_bit Number of bits to use for discretizing the levels
#' 
#' @return A numeric value
#' 
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
kittlerillingerThreshold <- function (input_image,nr_bit=8)  # use all channels?
{
  greyVec <- input_image
  totPixels <- length(greyVec)
  ## coding in 16 bit, slightly better
  greyLevels <- greyVec * (2^nr_bit -1) # parametrizable? nrBit <- 16
  # greyLevels <- greyVec * (2^8 -1) # parametrizable? nrBit <- 16
  #### modified!!
  greyLevels <- as.vector(greyLevels)
  greyLevels <- round(greyLevels)
  
  tableGrey <- tabulate(greyLevels)
  tableGrey <- tableGrey + 0.000001
  
  
  n_i <- tableGrey
  p_i <- n_i/totPixels
  # careful, sum is not exactly 1...
  
  
  P_t <- function(p, k)
  {
    s <- sum(p[0:k])
    return(s)
  }
  
  
  ##
  mean_f <- function(p, k, L)
  {
    ind <- 1:k
    s <- ind * p[1:k]
    z <- sum(s)
    return(z)
  }
  
  mean_b <- function(p, k, L)
  {
    ind <- (k+1):L
    s <- ind * p[(k+1):L]
    z <- sum(s)
    return(z)
  }
  
  sigma_f_t <- function(p, k, L)
  {
    s <- 0
    mu <- mean_f(p,k,L)
    for (i in 1:k)
    {
      s <- s + ( (i - mu)^2 * p[i] )
    }
    return(sqrt(s))
  }
  
  sigma_b_t <- function(p, k, L)
  {
    s <- 0
    mu <- mean_f(p,k,L)
    for (i in (k+1):L)
    {
      s <- s + ( (i - mu)^2 * p[i] )
    }
    return(sqrt(s))
  }
  
  
  ## define the function to optimize
  kittIll <- function(p, L, k)
  {
    P_t(p,k)*log(sigma_f_t(p,k,L)) + (1-P_t(p,k))*log(sigma_b_t(p,k,L)) - P_t(p,k)*log(P_t(p,k)) - (1-P_t(p,k))*log((1-P_t(p,k)))
  }
  
  ## actually do the optimization (max required)
  o <- optimize(kittIll, c(1,length(tableGrey)), maximum=FALSE, L=length(tableGrey), p=p_i )
  kittlerillinger_threshold <- o$minimum/((2^nr_bit)-1) 
  
  cat("Calculated value with Kittler-Illinger thresholding method...")
  cat(kittlerillinger_threshold)
  cat("\n")
  
  return(kittlerillinger_threshold)

}

