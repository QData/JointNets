
.checkInvk <- function(m) class(try(solve(m),silent=T))=="matrix"

.softThrek <- function(x, lambda){
  result = sign(x) * pmax(abs(x)-lambda, 0)
  return(result)
}

.hardThrek <- function(x, lambda){
  x[(x != diag(diag(x))) & (abs(x) < lambda)] <- 0
  return(x)
}

.backwardMapk <-function(covMatrix, thre = "soft"){
  niuList = 0.001 * (0:1000) * max(covMatrix)
  bestDet = det(.softThrek(covMatrix, 0.001))
  bestniu = 0.001

  if (thre == "soft"){
    for (i in 1:1000){
      if (bestDet < det(.softThrek(covMatrix, niuList[i]))){
        bestDet = det(.softThrek(covMatrix, niuList[i]))
        bestniu = niuList[i]
      }
    }
    return(solve(.softThrek(covMatrix, bestniu)))
  }

  if (thre == "hard"){
    for (i in 1:1000){
      if (.checkInvk(.hardThrek(covMatrix, niuList[i]))){
        bestniu = niuList[i]
        break
      }
    }
    return(solve(.hardThrek(covMatrix, bestniu)))
  }
}

#' Fast and Scalable Estimator for Using Additional Knowledge in Learning
#' Sparse Structure Change of High Dimensional of Sparse Changes
#' in High-Dimensional Gaussian Graphical Models
#'
#' The DIFFEEK algorithm
#'
#' @param C A input matrix for the 'control' group. It can be data matrix or
#' covariance matrix. If C is a symmetric matrix, the matrices are assumed to
#' be covariance matrix.
#' @param D A input matrix for the 'disease' group. It can be data matrix or
#' covariance matrix. If D is a symmetric matrix, the matrices are assumed to
#' be covariance matrix.
#' @param W positive weight matrix of size p x p representing prior knowledge of the graphs
#' @param g grouping information
#' (a vector of size p representing node groups, eg: c(1,1,2,2,0,0)
#' represents node 1&2 are in group 1, 3&4 are in group 1, 5&6 are not in any group)
#' @param epsilon A positive number. The hyperparameter controls the sparsity level of the
#' groups in g of the difference matrix
#' @param lambda A positive number. The hyperparameter controls the sparsity level of the difference matrix
#' @param covType A parameter to decide which Graphical model we choose to
#' estimate from the input data.
#'
#' If covType = "cov", it means that we estimate multiple sparse Gaussian
#' Graphical models. This option assumes that we calculate (when input X
#' represents data directly) or use (when X elements are symmetric representing
#' covariance matrices) the sample covariance matrices as input to the simule
#' algorithm.
#'
#' If covType = "kendall", it means that we estimate multiple nonparanormal
#' Graphical models. This option assumes that we calculate (when input X
#' represents data directly) or use (when X elements are symmetric representing
#' correlation matrices) the kendall's tau correlation matrices as input to the
#' simule algorithm.
#' @param thre A parameter to decide which threshold function to use for
#' \eqn{T_v}. If thre = "soft", it means that we choose soft-threshold function
#' as \eqn{T_v}. If thre = "hard", it means that we choose hard-threshold
#' function as \eqn{T_v}.
#' @return \item{diffNet}{A matrix of the estimated sparse changes between two
#' Gaussian Graphical Models}
#' @author Beilun Wang
#' @export
#' @examples
#' \dontrun{
#' library(JointNets)
#' data(exampleData)
#' result = diffeek(exampleData[[1]], exampleData[[2]],
#' W = matrix(1,20,20), g = rep(0,20),epsilon = 0.2,
#' lambda = 0.4,covType = "cov")
#' plot(result)
#' }
#' @import pcaPP
#' @importFrom stats cov
diffeek <- function(C, D, W, g, epsilon = 1, lambda = 0.05, covType = "cov", thre = "soft"){

  if (is.data.frame(C)){
    C = as.matrix(C)
  }

  if (is.data.frame(D)){
    D = as.matrix(D)
  }
  if (covType == "cov") {
    if (isSymmetric(C) == FALSE){
      covX = cov(C)
    }
    else{
      covX = C
    }

    if (isSymmetric(D) == FALSE){
      covY = cov(D)
    }
    else{
      covY = D
    }
  }

  if (covType == "kendall") {
    if (isSymmetric(C) == FALSE){
      covX = cor.fk(C)
    }
    else{
      covX = C
    }

    if (isSymmetric(D) == FALSE){
      covY = cor.fk(D)
    }
    else{
      covY = D
    }
  }

  backX = .backwardMapk(covX, thre)
  backY = .backwardMapk(covY, thre)
  B = backY -backX
  diffNet = .softThrek(B, lambda/W)
  diag(diffNet) = 0

  if (epsilon > 0 & max(g) != 0){
    for (i in 1:max(g)){
      index = which(g == i)
      B2 = max(norm(B[index,index], 'F') - epsilon * lambda, 0) * B[index,index] / norm(B[index,index], 'F')
      diffNet[index,index] = pmax(lambda - B[index,index], pmin(B2, lambda + B[index,index]))
    }
  }

  diag(diffNet) = 1
  diffNet = list(diffNet)
  ### share = NULL since diffeek produces only the difference graph
  out = list(graphs = diffNet, share = NULL)
  class(out) = "diffeek"
  out = add_name_to_out(out,C)
  return(out)
}




