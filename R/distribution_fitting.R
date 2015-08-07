
#################################################
# Automatic distribution fitting and selection
#
# Copyright 2014 worldofpiggy.com
#################################################
library(MASS)
#Usage: 
#data, numeric vector of observations of unknown distribution
#fit, a list of distributions to fit data
#sample, rate of subsampling (0.5 means that a sample 50% of data will be considered) 

#' Function to fit distribution to data
#' Provides test results from fitting distributions to data
#' 
#' @param data A numeric vector containing the values to fit
#' @param fit Names of the distributions to fit. Distributions "beta", "cauchy", "chi-squared", "exponential", "f", "gamma", "geometric", "lognormal", "logistic", "negative-binomial", "normal", "Poisson", "t" and "weibull" are recognised, case being ignored.
#' @param sample The proportion of the data to be used for fitting. A random sample will be taken. Default is 1.
#' @param method Method to use for fitting. "mle" for 'maximum likelihood estimation', "mme" for 'moment matching estimation', "qme" for 'quantile matching estimation' and "mge" for 'maximum goodness-of-fit estimation'.
#' @return A dataframe containing fitted parameters and test statistics
#' @examples
#' fit_distribution(data, c("poisson", "negative-binomial"))
fit_distribution <- function(data, fit="gamma", sample = 1, method = "mle"){

  numfit <- length(fit)
  fit <- tolower(fit)
                 
  # function for individual distribution
  indiv_fit <- function(data, fit, sample = 1){
# Gamma-----------------------------------------------------------
  
  # take a sample of dataset
  n = round(length(data)*sample)
  data = sample(data, size=n, replace=F)

  if(fit == "gamma") {
    gf_shape = "gamma"
    fd_g <- fitdist(data, "gamma",method = method)
    est_shape = fd_g$estimate[[1]]
    est_rate = fd_g$estimate[[2]]
    
    ks = ks.test(data, "pgamma", shape=est_shape, rate=est_rate)
    
    # add to results
    retVal = c(gf_shape, est_shape, est_rate, ks$statistic, ks$p.value)
  }
  
# Poisson-----------------------------------------------------------
    
    else if(fit == "poisson"){
      gf_shape = "poisson"
      fd_p <- fitdist(data, "poisson", method = method)
      est_lambda = fd_p$estimate[[1]]
      
      samps <- rpois(200, est_lambda)
      ks = ks.boot(data, samps)
      # add to results
      retVal = c(gf_shape, est_lambda, NA, NA, ks$ks.boot.pvalue)
    }

# Weibull-----------------------------------------------------------
    
    else if(fit == "weibull"){
      gf_shape = "weibull"
      fd_w <- fitdist(data,"weibull", method = method)
      est_shape = fd_w$estimate[[1]]
      est_scale = fd_w$estimate[[2]]
      
      ks = ks.test(data, "pweibull", shape=est_shape, scale=est_scale)
      # add to results
      retVal = c(gf_shape, est_shape, est_scale, ks$statistic, ks$p.value) 
    }
    
# Normal-----------------------------------------------------------
    
    else if(fit == "normal"){
      gf_shape = "normal"
      fd_n <- fitdistr(data, "normal", method = method)
      est_mean = fd_n$estimate[[1]]
      est_sd = fd_n$estimate[[2]]
      
      ks = ks.test(data, "pnorm", mean=est_mean, sd=est_sd)
      # add to results
      retVal = c(gf_shape, est_mean, est_sd, ks$statistic, ks$p.value)
    }

# Exponential-----------------------------------------------------------
    
    else if(fit == "exponential"){
      gf_shape = "exponential"
      fd_e <- fitdist(data, "exponential", method = method)
      est_rate = fd_e$estimate[[1]]
      ks = ks.test(data, "pexp", rate=est_rate)
      # add to results
      retVal = c(gf_shape, est_rate, "NA", ks$statistic, ks$p.value)
    }
 
# Logistic-----------------------------------------------------------
    
    else if(fit == "logistic"){
      gf_shape = "logistic"
      fd_l <- fitdist(data, "logistic", method = method)
      est_location = fd_l$estimate[[1]]
      est_scale = fd_l$estimate[[2]]
      ks = ks.test(data, "plogis", location=est_location, scale=est_scale)
      # add to results
      retVal = c(gf_shape, est_location, est_scale, ks$statistic, ks$p.value) 
    }
    
# Lognormal-----------------------------------------------------------
    
    else if(fit == "lognormal"){
      gf_shape = "lognormal"
      fd_l <- fitdist(data, "lognormal", method = method)
      mean_log = fd_l$estimate[[1]]
      sd_log = fd_l$estimate[[2]]
      ks = ks.test(data, "plnorm", meanlog = mean_log, sdlog=sd_log)
      # add to results
      retVal = c(gf_shape, mean_log, sd_log, ks$statistic, ks$p.value) 
    }
 
# Beta-----------------------------------------------------------
    
    else if(fit == "beta"){
      gf_shape = "beta"
      fd_l <- fitdistrplus::fitdist(data, "beta", method = method)
      est_1 = fd_l$estimate[[1]]
      est_2 = fd_l$estimate[[2]]
      ks = ks.test(data, "pbeta", shape1 = est_1, shape2 = est_2)
      # add to results
      retVal = c(gf_shape, est_1, est_2, ks$statistic, ks$p.value) 
    }   

# Cauchy-----------------------------------------------------------
    
    else if(fit == "cauchy"){
      gf_shape = "cauchy"
      fd_l <- fitdist(data, "cauchy", method = method)
      est_1 = fd_l$estimate[[1]]
      est_2 = fd_l$estimate[[2]]
      ks = ks.test(data, "pcauchy", location = est_1, scale = est_2)
      # add to results
      retVal = c(gf_shape, est_1, est_2, ks$statistic, ks$p.value) 
    }     
    
# Chi-squared-----------------------------------------------------------
    
    else if(fit == "chi-squared"){
      gf_shape = "chi-squared"
      fd_l <- fitdistrplus::fitdist(data, "chisq", start = list(df=7), method = method)
      est_1 = fd_l$estimate[[1]]
      
      ks = ks.test(data, "pchisq", df = est_1)
      # add to results
      retVal = c(gf_shape, est_1, NA, ks$statistic, ks$p.value) 
    }   
    
# F -----------------------------------------------------------
    
    else if(fit == "f"){
      gf_shape = "f"
      fd_l <- fitdistrplus::fitdist(data, "f", start = list(df1=7, df2=9), method = method)
      est_1 = fd_l$estimate[[1]]
      est_2 = fd_l$estimate[[2]]
      
      ks = ks.test(data, "pf", df1 = est_1, df2=est_2)
      # add to results
      retVal = c(gf_shape, est_1, est_2, ks$statistic, ks$p.value) 
    }  
    
# Negative Binomial -----------------------------------------------------------
    
    else if(fit == "negative-binomial"){
      gf_shape = "negative-binomial"
      fd_l <- fitdistrplus::fitdist(data, "nbinom", method = method)
      est_1 = fd_l$estimate[[1]]
      est_2 = fd_l$estimate[[2]]
      
      samps <- rnbinom(200, size = est_1, mu=est_2)
      ks = ks.boot(data, samps)
      # add to results
      retVal = c(gf_shape, est_1, est_2, NA, ks$ks.boot.pvalue) 
    }  
    
# T -----------------------------------------------------------
    
    else if(fit == "t"){
      gf_shape = "t"
      fd_l <- fitdistrplus::fitdist(data, "t", start=list(df=4), method = method)
      est_1 = fd_l$estimate[[1]]

      
      ks = ks.test(data, "pt", df = est_1)
      # add to results
      retVal = c(gf_shape, est_1, NA, ks$statistic, ks$p.value) 
    }  
    
  
    retVal = data.frame(distribution = retVal[1],
                        param_1 = as.numeric(retVal[2]),
                        param_2 = as.numeric(retVal[3]),
                        ks_statistic = as.numeric(retVal[4]),
                        ks_p_value = as.numeric(retVal[5]))
  
    return(retVal)    
  }
    

  retVal <- do.call("rbind", lapply(1:numfit, function(i){
    tryCatch(suppressWarnings(indiv_fit(data,fit[i], sample)),
             error = function(e){
               return(data.frame(distribution = fit[i],
                                 param_1 = NA,
                                 param_2=NA,
                                 ks_statistic = NA,
                                 ks_p_value = NA))
             })
  }))


  return(retVal)
}


