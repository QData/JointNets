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

#' Fast and Scalable Learning of Sparse Changes in High-Dimensional Gaussian
#' Graphical Model Structure
#'
#'
#' The DIFFEEK algorithm
#'
#' @param C A input matrix for the 'control' group. It can be data matrix or
#' covariance matrix. If C is a symmetric matrix, the matrices are assumed to
#' be covariance matrix. More details at <https://github.com/QData/DIFFEE>
#' @param D A input matrix for the 'disease' group. It can be data matrix or
#' covariance matrix. If D is a symmetric matrix, the matrices are assumed to
#' be covariance matrix. More details at <https://github.com/QData/DIFFEE>
#' @param w weight matrix
#' @param g grouping information
#' @param epsilon A positive number
#' @param lambda A positive number. The hyperparameter controls the sparsity
#' level of the matrices. The \eqn{\lambda_n} in the following section:
#' Details.
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
#' @references Beilun Wang, Arshdeep Sekhon, Yanjun Qi (2018). Fast and
#' Scalable Learning of Sparse Changes in High-Dimensional Gaussian Graphical
#' Model Structure. <arXiv:1710.11223>
#' @export
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

  if (covType == "cor") {
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

  diffNet = matrix(0,nrow(W),ncol(W)) ## initialize empty diffNet
  gV = cbind(combn(g,2),combn(g,2)[c(2,1),]) ## obtain gV

  ## update diffnet on E\gV
  for (i in 1:nrow(W)){
    for (j in 1:ncol(W)){
      diffNet[i,j] = .softThrek(B[i,j], lambda / W[i,j])
    }
  }

  ## update diffnet on gV
  sum = 0
  for (i in 1:dim(gV)[2]){
      sum = sum + (B[gV[1,i],gV[2,i]])^2
  }
  B_V_norm = sqrt(sum)
  B2 = max(B_V_norm - epsilon * lambda, 0) * B / B_V_norm
  for (i in 1:dim(gV)[2]){
    a = gV[1,i]
    b = gV[2,i]
    diffNet[a,b] = pmax(lambda - B[a,b], pmin(B2[a,b], lambda + B[a,b]))
  }

  diffNet = list(diffNet)
  ### share = NULL since diffeek produces only the difference graph
  out = list(graphs = diffNet, share = NULL)
  class(out) = "diffeek"
  out = add_name_to_out(out,C)
  return(out)
}





