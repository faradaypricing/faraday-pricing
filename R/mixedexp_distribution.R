
#' Returns the density of the mixed exponential distribution specified with
#' the given weights and means.
#' 
#'
#' @param x Vector of quantiles
#' @param weights The weights attached to each of the individual exponential distributions
#' @param means The means of the individual exponential distributions
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @return The density of the mixed exponential distribution
#' @examples
#' dmixedexp(c(10,20), c(0.3,0.6,0.1), c(5,8,10))
dmixedexp <- function(x, weights, means, log= F){
  
  if(length(weights) != length(means)){
    stop("The weights and means vectors cannot have different lengths")
  }
  
  weights_adj <- weights / sum(weights)
  
  retVal <- sapply(x, function(value){
    sum(weights_adj/means * exp(-value/means))
  })
  
  if(log){
    retVal = log(retVal)
  }
  
  return(retVal)

}

#' Returns the CDF value of the mixed exponential distribution specified with
#' the given weights and means.
#' 
#'
#' @param q Vector of quantiles
#' @param weights The weights attached to each of the individual exponential distributions
#' @param means The means of the individual exponential distributions
#' @param lower.tail logical; if TRUE (default), probabilities are P[X ≤ x], otherwise, P[X > x].
#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#' @return The CDF of the mixed exponential distribution
#' @examples
#' pmixedexp(c(10,20), c(0.3,0.6,0.1), c(5,8,10))
pmixedexp <- function(q, weights, means, lower.tail = TRUE, log.p = FALSE){
  
  if(length(weights) != length(means)){
    stop("The weights and means vectors cannot have different lengths")
  }
  
  weights_adj <- weights / sum(weights)
  
  retVal <- sapply(q, function(value){
    sum(weights_adj * exp(-value/means))
  })
  
  if(lower.tail){
    retVal <- 1 - retVal
  }
  
  if(log.p){
    retVal <- log(retVal)
  }
  
  return(retVal)
  
}

#' Returns random samples from the mixed exponential distribution specified with
#' the given weights and means
#' 
#'
#' @param n A character vector of ISO currency codes
#' @param weights The weights attached to each of the individual exponential distributions
#' @param means The means of the individual exponential distributions
#' @return n random samples from the mixed exponential distribution
#' @examples
#' rmixedexp(100, c(0.3,0.5,0.2), c(10,15,24))
rmixedexp <- function(n, weights, means){
  
  if(length(weights) != length(means)){
    stop("The weights and means vectors cannot have different lengths")
  }
  
  weights_adj <- weights / sum(weights)
  
  
  samps <- sample(x = 1:length(weights), size = n, replace = T, prob = weights_adj)
  
  retVal <- sapply(samps, function(index){
    r <- rexp(n = 1, rate = 1/means[index])
  })
  
  return(retVal)
}

#' Returns the quantile function value of the mixed exponential distribution specified with
#' the given weights and means.
#' 
#'
#' @param p Vector of probabilities
#' @param weights The weights attached to each of the individual exponential distributions
#' @param means The means of the individual exponential distributions
#' @param lower.tail logical; if TRUE (default), probabilities are P[X ≤ x], otherwise, P[X > x].
#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#' @return The quantile function value of the mixed exponential distribution
#' @examples
#' pmixedexp(c(0.6, 0.75, 0.9, 0.99), c(0.3, 0.6, 0.1), c(5, 8, 10))
qmixedexp <- function(p, weights, means, lower.tail = TRUE, log.p = FALSE){
  
  func <- function(x){
    pmixedexp(x, weights = weights, means = means, lower.tail = lower.tail, log.p = log.p) - p
  }
  
  x <- rootSolve::uniroot.all(f = func, interval = c(0,1E10))
  
  return(x)
}

#' Returns the mean of the mixed exponential distribution specified with
#' the given weights and means, with values limited to limit.
#' 
#'
#' @param limit Vector of limits
#' @param weights The weights attached to each of the individual exponential distributions
#' @param means The means of the individual exponential distributions
#' @examples
#' levmixedexp(c(19,22), c(0.3, 0.5, 0.3), c(10, 4, 2))
levmixedexp <- function(limit, weights, means){
  
  if(length(weights) != length(means)){
    stop("The weights and means vectors cannot have different lengths")
  }
  
  weights_adj <- weights / sum(weights)
  
  lambdas <- 1/means
  
  retVal <- sapply(limit, function(l){
    
    lev <- sapply(1:length(weights), function(i){
      x <- weights_adj[i] * levexp(l, lambdas[i], order = 1)
      
    })
    
    return(sum(lev))
  })
  
  return(retVal)
  
  
}
