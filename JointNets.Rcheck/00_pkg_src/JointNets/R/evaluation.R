#' function to plot comparsion between simulation and result
#' @param simulate simulated result from simulateGraph
#' @param result result generated from any of the jointnet methods
#' @export
comparisonplot <-
  function(simulate,
           result)
  {
    K = length(simulate$graphs)
    graphics.off()
    par(ask = F)
    par(mfrow = c(2, K + 1))
    graph = returngraph(result)
    layout = layout_nicely(graph, dim = 2)
    for (i in 1:K) {
      plot(
        simulate,
        type = "taskspecific",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )
    }
    plot(simulate, type = "share", layout = layout)


    for (i in 1:K) {
      plot(
        result,
        type = "taskspecific",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )
    }
    plot(result, type = "share", layout = layout)
  }


#author: Beilun Wang
#input:
#covs   -- a list of estimated covariance matrices
#graphs -- a list of estimated precision matrices
#n      -- total number of samples
#p      -- number of features
#output:
#bic    -- bic number
#' function to calculate BIC score of a list of estimated precision matrices
#' @export
BIC <- function(graphs, n, p){
  N = length(graphs)
  covs= list()
  ### not sure if covs is the empirical covariance matrix or the inverse precision matrix
  for (i in 1:N){
  covs[[i]] = solve(graphs[[i]])
  }
  bic = 0
  #calculate loglikelihood
  for(i in 1:N){
    bic = determinant(graphs[[i]])$modulus[1] - sum(diag(covs[[i]] %*% graphs[[i]])) + bic
  }
  #calculate bic
  bic = -2 * bic + ((p * p - p) / 2 + p) * log(n)
  return(bic)
}

#' function to calculate F1 score
#' @export
F1 <- function(simulate, result) {
  K = length(simulate$graphs)
  simulate = make_adj_matrix(simulate)
  result = make_adj_matrix(result)
  f1 = list()
  for (i in 1:K) {
    tP = sum((result[[i]] == 1) & (simulate[[i]] == 1))
    tN = sum((result[[i]] == 0) & (simulate[[i]] == 0))
    fP = sum((result[[i]] == 1) & (simulate[[i]] == 0))
    fN = sum((result[[i]] == 0) & (simulate[[i]] == 1))
    pPM = tP / (tP + fP)
    rPM = tP / (tP + fN)
    f1[[i]] = 2 * pPM * rPM / (pPM + rPM)
  }
  return(f1)
}
