


#' Calculate the following probabilities for a nil claim at a certain development period:
#' - the probability that it will be nil at the next development
#' - the probability that it will be nil at the maximum development period
#' 
NilClaimsSurvivalProbabilities <- function(claim.data, ref, dev, value,group = NULL){
  
  # copy data
  data = claim.data
  
  # set up single fake group if data isn't grouped
  if(is.null(group)){
    group.col = 1
  } else{
    group.col = data[,group]
  }
  data = data.frame(dev=data[,dev], ref=data[,ref], group = group.col, value = data[,value])
  data$nil <- ifelse(data$value > 0 , 0 , 1)
  
  # drop claim value column
  data <- data[,colnames(data) != "value"]
  
  #match next 
  data.next = data
  data.next$dev <- data[[dev]] - 1
  
  cols <- colnames(data)[which(colnames(data) != "nil")]
  data <- merge(data, data.next, by = cols)
  
  # sum counts to enable probability estimate
  data <- data %>% select(dev, group, nil.x, nil.y) %>% group_by( group, dev) %>% summarise_each(funs(sum))
  data$inc.prob <- 1
  data$inc.prob <- ifelse(data$nil.x==0, 1, data$nil.y / data$nil.x)
  
  # cumulative probability assuming independence between periods
  data = data %>% arrange(desc(dev))
  data= data %>% mutate(cum.prob = cumprod(inc.prob))
  
  if(is.null(group)){
    retVal = data.frame(dev = data$dev, inc.prob=data$inc.prob, cum.prob=data$cum.prob)
    retVal = retVal %>% arrange(dev)
    colnames(retVal) <- c(dev,"inc.prob","cum.prob")
  } else {
    retVal = data.frame(group = data$group, dev = data$dev, inc.prob=data$inc.prob, cum.prob=data$cum.prob)
    retVal = retVal %>% arrange(group,dev)
    colnames(retVal) <- c(group,dev,"inc.prob","cum.prob")    
  }
  retVal 
}

NilClaimsSurvivalProbs <- function(x, y, group = NULL){
  
  # copy data
  data = sign(as.numeric(x))
  
  # set up single fake group if data isn't grouped
  if(is.null(group)){
    group.col = 1
  } else{
    group.col = data[,group]
  }
  data = data.frame(dev=data[,dev], ref=data[,ref], group = group.col, value = data[,value])
  data$nil <- ifelse(data$value > 0 , 0 , 1)
  
  # drop claim value column
  data <- data[,colnames(data) != "value"]
  
  #match next 
  data.next = data
  data.next$dev <- data[[dev]] - 1
  
  cols <- colnames(data)[which(colnames(data) != "nil")]
  data <- merge(data, data.next, by = cols)
  
  # sum counts to enable probability estimate
  data <- data %>% select(dev, group, nil.x, nil.y) %>% group_by( group, dev) %>% summarise_each(funs(sum))
  data$inc.prob <- 1
  data$inc.prob <- ifelse(data$nil.x==0, 1, data$nil.y / data$nil.x)
  
  # cumulative probability assuming independence between periods
  data = data %>% arrange(desc(dev))
  data= data %>% mutate(cum.prob = cumprod(inc.prob))
  
  if(is.null(group)){
    retVal = data.frame(dev = data$dev, inc.prob=data$inc.prob, cum.prob=data$cum.prob)
    retVal = retVal %>% arrange(dev)
    colnames(retVal) <- c(dev,"inc.prob","cum.prob")
  } else {
    retVal = data.frame(group = data$group, dev = data$dev, inc.prob=data$inc.prob, cum.prob=data$cum.prob)
    retVal = retVal %>% arrange(group,dev)
    colnames(retVal) <- c(group,dev,"inc.prob","cum.prob")    
  }
  retVal 
}