#' reduce the dimensionality of the datalist if needed
#' @param datalist a datalist of high dimensionality
#' @return a datalist of reduced dimensionality
#' @importFrom stats sd
#' @export
#' @examples
#' library(JointNets)
#' data(exampleData)
#' reduction = dimension_reduce(exampleData)
dimension_reduce <- function(datalist){
  ## min number of n across all contexts
  min_n = min(do.call(rbind,lapply(datalist, dim))[,1])
  p = dim(datalist[[1]])[2]

  ## reduce p only if necessary
  if (p <= 2*min_n){
    return(datalist)
  }
  else {
    temp = do.call(rbind, datalist)
    lists = split(temp, rep(1:ncol(temp), each = nrow(temp)))
    standard_deviation = sapply(lists, sd)
    standard_deviation = unlist(standard_deviation)
    bound = 2 * min_n
    indexes = order(standard_deviation,decreasing = TRUE)[1: bound]

    ## reduce p to 2 * min_n
    result = lapply(datalist, function(x,min_n) { x[,min_n] }, min_n = indexes)
    return(result)
  }
}

