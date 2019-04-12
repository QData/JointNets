#'helper function to add compute covariance matrix / kendall tau correlation matrix
#'@param X data matrix
#'@param covType "cov" or "kendall"
#'@return covriance matrix / kendall tau correlation matrix
compute_cov <- function(X,covType = "cov"){

  result = X
  if (is.data.frame(X)){
    result = as.matrix(X)
  }
  if (!isSymmetric(X)){
    try(if (covType %in% c("cov","kendall") == FALSE) stop("The cov/cor type you specifies is not include in this package. Please use your own function to obtain the list of cov/cor and use them as the input of simule()"))
    if (covType == "cov"){
        result= stats::cov(result)
    }
    if (covType == "kendall"){
      result= cor.fk(result)
    }
  }
  return(result)
}

