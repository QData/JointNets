.checkInv <- function(m) class(try(solve(m),silent=T))=="matrix"

.softThre <- function(x, lambda){
    result = sign(x) * pmax(abs(x)-lambda, 0)
    result
}

.hardThre <- function(x, lambda){
    x[(x != diag(diag(x))) & (abs(x) < lambda)] <- 0
    return(x)
}

.backwardMap <-function(covMatrix, thre = "soft"){
    niuList = 0.001 * (0:1000) * max(covMatrix)
    bestDet = det(.softThre(covMatrix, 0.001))
    bestniu = 0.001

    if (thre == "soft"){
      for (i in 1:1000){
        if (bestDet < det(.softThre(covMatrix, niuList[i]))){
          bestDet = det(.softThre(covMatrix, niuList[i]))
          bestniu = niuList[i]
        }
      }
      return(solve(.softThre(covMatrix, bestniu)))
    }

    if (thre == "hard"){
      for (i in 1:1000){
        if (.checkInv(.hardThre(covMatrix, niuList[i]))){
          bestniu = niuList[i]
          break
        }
      }
      return(solve(.hardThre(covMatrix, bestniu)))
    }
}


#' Fast and Scalable Learning of Sparse Changes in High-Dimensional Gaussian
#' Graphical Model Structure
#'
#' Estimate DIFFerential networks via an Elementary Estimator under a
#' high-dimensional situation. Please run demo(diffee) to learn the basic
#' functions provided by this package.  For further details, please read the
#' original paper: Beilun Wang, Arshdeep Sekhon, Yanjun Qi (2018)
#' <arXiv:1710.11223>.
#'
#' The DIFFEE algorithm is a fast and scalable Learning algorithm of Sparse
#' Changes in High-Dimensional Gaussian Graphical Model Structure. It solves
#' the following equation: \deqn{ \min\limits_{\Delta}||\Delta||_1 } Subject to
#' : \deqn{ ([T_v(\hat{\Sigma}_{d})]^{-1} -
#' [T_v(\hat{\Sigma}_{c})]^{-1})||_{\infty} \le \lambda_n } Please also see the
#' equation (2.11) in our paper. The \eqn{\lambda_n} is the hyperparameter
#' controlling the sparsity level of the matrix and it is the \code{lambda} in
#' our function. For further details, please see our paper: Beilun Wang,
#' Arshdeep Sekhon, Yanjun Qi (2018) <arXiv:1710.11223>.
#'
#' @param C A input matrix for the 'control' group. It can be data matrix or
#' covariance matrix. If C is a symmetric matrix, the matrices are assumed to
#' be covariance matrix. More details at <https://github.com/QData/DIFFEE>
#' @param D A input matrix for the 'disease' group. It can be data matrix or
#' covariance matrix. If D is a symmetric matrix, the matrices are assumed to
#' be covariance matrix. More details at <https://github.com/QData/DIFFEE>
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
#' @examples
#' \dontrun{
#' library(JointNets)
#' data(exampleData)
#' result = diffee(exampleData[[1]], exampleData[[2]], 0.45)
#' plot(result)
#' }
diffee <- function(C, D, lambda = 0.05, covType = "cov", thre = "soft"){

    if (is.data.frame(C)){
      C = as.matrix(C)
    }

    if (is.data.frame(D)){
      D = as.matrix(D)
    }
    if (covType == "cov") {
      if (isSymmetric(C) == FALSE){
        covX = stats::cov(C)
      }
      else{
        covX = C
      }

      if (isSymmetric(D) == FALSE){
        covY = stats::cov(D)
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


    backX = .backwardMap(covX, thre)
    backY = .backwardMap(covY, thre)
    diffNet = .softThre((backY - backX), lambda)
    ### change to just a simple list output
    out = diffNet
    class(out) = "diffee"
    return(out)
}


