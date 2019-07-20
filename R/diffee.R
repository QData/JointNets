.checkInv <- function(m) class(try(solve(m),silent=T))=="matrix"

.softThre <- function(x, lambda){
    result = sign(x) * pmax(abs(x)-lambda, 0)
    return(result)
}

.hardThre <- function(x, lambda){
    x[(x != diag(diag(x))) & (abs(x) < lambda)] <- 0
    return(x)
}

diffee.backwardMap <-function(covMatrix, thre = "soft"){
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
#' Graphical Model
#'
#' Estimate DIFFerential networks via an Elementary Estimator under a
#' high-dimensional situation. Please run demo(diffee) to learn the basics.
#' For further details, please read the
#' original paper: Beilun Wang, Arshdeep Sekhon, Yanjun Qi (2018)
#' <https://arxiv.org/abs/1710.11223>.
#'
#' The DIFFEE algorithm is a fast and scalable Learning algorithm of Sparse
#' Changes in High-Dimensional Gaussian Graphical Model Structure. It solves
#' the following equation: \deqn{ \min\limits_{\Delta}||\Delta||_1 } Subject to
#' : \deqn{ ([T_v(\hat{\Sigma}_{d})]^{-1} -
#' [T_v(\hat{\Sigma}_{c})]^{-1})||_{\infty} \le \lambda_n } Please also see the
#' equation (2.11) in our paper. The \eqn{\lambda_n} is the hyperparameter
#' controlling the sparsity level of the matrix and it is the \code{lambda} in
#' our function. For further details, please see our paper: Beilun Wang,
#' Arshdeep Sekhon, Yanjun Qi (2018) <https://arxiv.org/abs/1710.11223>.
#'
#' @param C A input matrix for the 'control' group. It can be data matrix or
#' covariance matrix. If C is a symmetric matrix, the matrices are assumed to
#' be covariance matrix.
#' @param D A input matrix for the 'disease' group. It can be data matrix or
#' covariance matrix. If D is a symmetric matrix, the matrices are assumed to
#' be covariance matrix.
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
#' @param intertwined indicate whether to use intertwined covariance matrix
#' @param thre A parameter to decide which threshold function to use for
#' \eqn{T_v}. If thre = "soft", it means that we choose soft-threshold function
#' as \eqn{T_v}. If thre = "hard", it means that we choose hard-threshold
#' function as \eqn{T_v}.
#' @return \item{$graphs}{A matrix of the estimated sparse changes between two
#' Gaussian Graphical Models} \item{$share}{null}
#' @author Beilun Wang
#' @references Beilun Wang, Arshdeep Sekhon, Yanjun Qi (2018). Fast and
#' Scalable Learning of Sparse Changes in High-Dimensional Gaussian Graphical
#' Model Structure. <https://arxiv.org/abs/1710.11223>
#' @export
#' @import pcaPP
#' @details if labels are provided in the datalist as column names, result will contain labels (to be plotted)
#' @examples
#' library(JointNets)
#' data(exampleData)
#' result = diffee(exampleData[[1]], exampleData[[2]], 0.45)
#' plot(result)

diffee <- function(C, D, lambda = 0.05, covType = "cov", intertwined = FALSE, thre = "soft"){

    covX = compute_cov(C,covType)
    covY = compute_cov(D,covType)

    if (intertwined){
      temp = intertwined(list(covX,covY),covType = covType)
      covX = temp[[1]]
      covY = temp[[2]]
    }

    backX = diffee.backwardMap(covX, thre)
    backY = diffee.backwardMap(covY, thre)
    diffNet = .softThre((backY - backX), lambda)
    diffNet = list(diffNet)
    ### share = NULL since diffee produces only the difference graph
    out = list(graphs = diffNet, share = NULL)
    class(out) = "diffee"
    out = add_name_to_out(out,C)
    return(out)
}


