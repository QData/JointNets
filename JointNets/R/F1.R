
#' This function computes F1 score for jointnet result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param result output generated from any one of the jointnet algorithms
#' @param ... additional arguments
#' @return computed F1 scores
#' @examples
#' \dontrun{
#' }
#' @export
F1 <- function(result, simulatedgraphs, ...) {
  UseMethod("F1", result)
}

#' @method F1 simule
#' @export
#' @export F1.simule
F1.simule <-
  function(result,
           simulatedgraphs,
           ...)
  {
    K = length(simulatedgraphs$graphs)
    f1_shared = F1_single((abs(simulatedgraphs$share)>1e-5)*1,
                          (abs(result$share)>1e-5)*1)
    simulatedgraphs = make_adj_matrix(simulatedgraphs,TRUE)
    result = make_adj_matrix(result,TRUE)
    f1 = c()
    for (i in 1:K) {
      f1[i] = F1_single(simulatedgraphs[[i]],result[[i]])
    }
    out = list(graphs = f1, share = f1_shared)
    return(out)
  }

#' @method F1 wsimule
#' @export
#' @export F1.wsimule
F1.wsimule <-
  function(result,
           simulatedgraphs,
           ...)
  {
    K = length(simulatedgraphs$graphs)
    f1_shared = F1_single((abs(simulatedgraphs$share)>1e-5)*1,
                          (abs(result$share)>1e-5)*1)
    simulatedgraphs = make_adj_matrix(simulatedgraphs,TRUE)
    result = make_adj_matrix(result,TRUE)
    f1 = c()
    for (i in 1:K) {
      f1[i] = F1_single(simulatedgraphs[[i]],result[[i]])
    }
    out = list(graphs = f1, share = f1_shared)
    return(out)
  }

#' @method F1 jeek
#' @export
#' @export F1.jeek
F1.jeek <-
  function(result,
           simulatedgraphs,
           ...)
  {
    K = length(simulatedgraphs$graphs)
    simulatedgraphs = make_adj_matrix(simulatedgraphs,TRUE)
    result = make_adj_matrix(result,TRUE)
    f1 = c()
    for (i in 1:K) {
      f1[i] = F1_single(simulatedgraphs[[i]],result[[i]])
    }
    out = list(graphs = f1)
    return(out)

  }

#' @method F1 fasjem
#' @export
#' @export F1.fasjem
F1.fasjem <-
  function(result,
           simulatedgraphs,
           ...)
  {
    K = length(simulatedgraphs$graphs)
    simulatedgraphs = make_adj_matrix(simulatedgraphs,TRUE)
    result = make_adj_matrix(result,TRUE)
    f1 = c()
    for (i in 1:K) {
      f1[i] = F1_single(simulatedgraphs[[i]],result[[i]])
    }
    out = list(graphs = f1)
    return(out)

  }

#' @method F1 diffee
#' @export
#' @export F1.diffee
F1.diffee <-
  function(result,
           simulatedgraphs,
           ...)
  {
    K = length(simulatedgraphs$graphs)
    simulatedgraphs = make_adj_matrix(simulatedgraphs,TRUE)
    difference = abs(simulatedgraphs[[1]] - simulatedgraphs[[2]])
    result = make_adj_matrix(result,TRUE)
    out = list(difference = F1_single(difference,result[[1]]))
    return(out)
  }

#' @method F1 diffeek
#' @export
#' @export F1.diffeek
F1.diffeek <-
  function(result,
           simulatedgraphs,
           ...)
  {
    K = length(simulatedgraphs$graphs)
    simulatedgraphs = make_adj_matrix(simulatedgraphs,TRUE)
    difference = abs(simulatedgraphs[[1]] - simulatedgraphs[[2]])
    result = make_adj_matrix(result,TRUE)
    out = list(difference = F1_single(difference,result[[1]]))
    return(out)
  }

#' function to calculate F1 score for a single simulate graph with a single result graph
#' @param truth ground truth precision matrix
#' @param estimate estimated precision matrix
#' @return a single F1 score between truth and estimate between 0 and 1 (higher scores correspond to better estimation)
#' @export
F1_single <- function(truth,estimate){
  tP = sum((estimate == 1) & (truth == 1))
  tN = sum((estimate == 0) & (truth == 0))
  fP = sum((estimate == 1) & (truth == 0))
  fN = sum((estimate == 0) & (truth == 1))
  pPM = tP / (tP + fP)
  rPM = tP / (tP + fN)
  return(2 * pPM * rPM / (pPM + rPM))
}
