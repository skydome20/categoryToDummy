#' Convert n 'category'('factor' and 'chr') variables to n-1 'dummy' variables of a data.frame 
#' with the concern of degree of freedom.
#' 
#' This function will convert 'category' variables to 'dummy' variables of a data.frame. 
#' In the concern about the issue of degree of freedom, 
#' each of the dummy coded variables having k groups will be converted in k-1 degrees of freedom.  
#'    
#' Traditionally, there are three steps to convert 'category' variables to 'dummy' variables of a data.frame:
#' 1. a data.frame must be divided as a data.frame with all 'category' variables and a data.frame with all 'non-category' variables.
#' 2. Then, calling the function 'model,matrix()' to convert 'category' variables to 'dummy' variables.
#' 3. Finally, combining 'dummy' variables with 'non-facotry' variables as a new data.frame for analyzing.
#' 
#' However, it's really annoying to do such thing below.
#' 
#' This function will handle an original data.frame by automatically identifying 'category' variables 
#' for 'dummy' variables converting, and remaining 'non-cateogry' variables.  
#' 
#' 
#' @author skydome20
#' @param data: data frame contains variables with any types (not support 'ordered-factor' type)
#' @return data frame 
#' @examples
#' 
#'  data1 <- iris
#'  result1 <- categoryToDummy(data1)
#'
#'  data2 <- data.frame(n1=c(1,8,5,3,6,4), n2=c(88,6,31,4,68,1), 
#'                      f1=c("a", "a", "c", "b", "b", "d"), f2=c("john", "kevin", "leon", "kevin", "leon", "john"))
#'  result2 <- categoryToDummy(data2)
#'  
#'  data3 <- VA
#'  result3 <- categoryToDummy(data3)
#'     
#'     

categoryToDummy <- function(data){
  # 'chr' to 'factor'
  data <- chrToFactor(data) 
  # extract the class of each column
  class.list <- lapply(data, class)
  # store colnames of a data frame
  colNames <- names(class.list)
  # create a vector of each column and its class
  colType <- unlist(class.list, use.names=T)
  # find which column is factor 
  factor.index <- unname(which(colType == "factor"))
  
  # means exist 'factor' variables
  if (is.integer0(factor.index) == F){
    
    # extract 'non-factor' variables
    nonfactor.df <- data[, -factor.index, drop = FALSE]
    
    # extract 'factor' variables
    factor.df <- data[, factor.index, drop = FALSE]
    
    # convert 'factor' variables to 'dummy' variables
    dummy.df <- as.data.frame(model.matrix(~., factor.df))
    dummy.df[, "(Intercept)"] <- NULL
    
    # Rename 'dummy' variables : 
    # since that model.matrix() will create new col.names by 
    # appending level to original col.names without any symbol, 
    # which is hard to tell original variables from levels. 
    a <- sapply(factor.df, function(x)levels(x), simplify=F)
    a <- lapply(a, `length<-`, max(lengths(a)))# adding NA to make all list elements equal length
    list.levels <- as.data.frame(a)
    list.levels <- list.levels[-1, ,drop=FALSE] # remove first level (since that model.matrix creates dummy matrix by ignoring the first level)
    new.dummy.names <- vector()
    for (i in 1:ncol(list.levels)){
      # append colnames with levels to create new colnames
      # the symbol used to separate 'colname' and 'level' is ':'
      newName <- paste(colnames(list.levels)[i], list.levels[is.na(list.levels[,i])==F, i], 
                       sep=":")
      new.dummy.names <-  append(new.dummy.names, newName)
    }
    
    # rename 
    colnames(dummy.df) <- new.dummy.names
    
    # combind 'non-factor' variables and 'dummy' variables
    data <- cbind(nonfactor.df, dummy.df)
  } 
  data # return result
}


#---------Required Functions----------#

  # get message 'integer(0)' and return (TRUE, FALSE) ####
  is.integer0 <- function(x){
    is.integer(x) && length(x) == 0L
  }
  
  # convert all 'character' columns to 'factor' columns in a data.frame ####
  chrToFactor <- function(data){
    tmp <- unclass(data)
    as.data.frame(tmp)
  }