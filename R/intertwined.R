## calculate intertwined covariance matrix
## @param X a list of covariance matrix
## @param covType = "cov"
## @return intertwined covariance matrices
intertwined <- function(X,covType = "cov"){
  if (covType == "cov"){
    cov.mean = cov(do.call(rbind, X))
  }
  if (covType == "kendall"){
    cov.mean = cor.fk(do.call(rbind, X))
  }
  N = length(X)
  for(i in 1:N){
    X[[i]] = 1/2 * X[[i]] + 1/2 * cov.mean
  }
  result = X
  return(result)
}

