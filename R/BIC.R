#' calculate BIC score for JointNets method
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param datalist datalist used as an input to any of the JointNets method
#' @param result result generated from datalist using the same JointNets method
#' @return BIC score
#' @export
#' @details not working with DIFFEE and kdiffnet (difference estimation)
#' @examples
#' library(JointNets)
#' simulateresult = simulation(p = 20,  n = c(100,100))
#' result = simule(simulateresult$simulatedsamples, 0.2, 0.5, covType = "cov", FALSE)
#' BIC(simulateresult$simulatedsamples,result)
BIC <- function(datalist, result){
  K = length(datalist)
  covs = list()
  n = 0
  for (i in 1:K){
    covs[[i]] = cov(datalist[[i]])
    n = n + dim(datalist[[i]])[1]
  }
  graphs = result$graphs
  p = dim(datalist[[i]])[2]
  return(BIC_matrix(covs,graphs,n,p))
}

## function to calculate BIC score of a list of estimated precision matrices
## @param covs   -- a list of empirical covariance matrices
## @param graphs -- a list of estimated precision matrices
## @param n      -- total number of samples (total number of samples from all lists)
## @param p      -- number of features
## @return BIC score
BIC_matrix <- function(covs, graphs, n, p){
  N = length(covs)
  bic = 0
  #calculate loglikelihood
  for(i in 1:N){
    bic = determinant(graphs[[i]])$modulus[1] - sum(diag(covs[[i]] %*% graphs[[i]])) + bic
  }
  #calculate bic
  bic = -2 * bic + ((p * p - p) / 2 + p) * log(n)
  return(bic)
}
