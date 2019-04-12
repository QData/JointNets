

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
#' @param intertwined indicate whether to use intertwined covariance matrix
#' @param thre A parameter to decide which threshold function to use for
#' \eqn{T_v}. If thre = "soft", it means that we choose soft-threshold function
#' as \eqn{T_v}. If thre = "hard", it means that we choose hard-threshold
#' function as \eqn{T_v}.
#' @return \item{diffNet}{A matrix of the estimated sparse changes between two
#' Gaussian Graphical Models}
#' @author Beilun Wang
#' @export
#' @examples
#' library(JointNets)
#' data(exampleData)
#' result = diffeek(exampleData[[1]], exampleData[[2]],
#' W = matrix(1,20,20), g = rep(0,20),epsilon = 0.2,
#' lambda = 0.4,covType = "cov")
#' plot(result)
#' @import pcaPP
#' @importFrom stats cov
diffeek <- function(C, D, W, g, epsilon = 1, lambda = 0.05, covType = "cov", intertwined = FALSE, thre = "soft"){

  covX = compute_cov(C,covType)
  covY = compute_cov(D,covType)

  if (intertwined){
    temp = intertwined(list(covX,covY),covType = covType)
    covX = temp[[1]]
    covY = temp[[2]]
  }

  # merge diffee and diffeek helper methods
  backX = .backwardMap(covX, thre)
  backY = .backwardMap(covY, thre)
  B = backY -backX
  diffNet = .softThre(B, lambda/W)
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




