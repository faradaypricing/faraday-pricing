


#' Returns a data frame that contains the original data from the input character vector split over multiple columns.
#'
#' @param char_vector A character vector
#' @param num_rows The number of rows in the resulting data frame
#' @return A data frame with the list wrapped over columns of length num_rows
#' @examples
#' split_into_columns(data, 12)
split_into_columns <- function(char_vector, num_rows){
  
  vec <- as.character(char_vector)
  if(length(vec) < num_rows){
    num_rows = length(vec)
  }
  
  ncols <- ceiling(length(vec) / num_rows)
  
  
  retVal <- do.call("cbind", lapply(1:ncols, function(n){
    
    start = (n-1)*num_rows + 1
    end = min(n*num_rows, length(vec))
    
    r <- vec[start:end]
    if(length(r) < num_rows){
      r <- c(r, rep("",num_rows - length(r)))
    }
    
    return(r)
  }))
  
  colnames(retVal) <- paste("col.", 1:ncols, sep="")
  return(retVal)
  
}
