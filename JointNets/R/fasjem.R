.norm_vec <- function(x)
  sqrt(sum(x ^ 2))
.norm_infty <- function(x)
  max(abs(x))

.f1 <- function(x, ga) {
  result = sign(x) * pmax(abs(x) - ga, 0)
  return(result)
}

.g1 <- function(x, a, lambda) {
  result = pmax(pmin(x - a, -lambda), lambda) + a
  return(result)
}

.f2 <- function(x, ga) {
  result = x * array(pmax(1 - ga / apply(x, c(1, 2), .norm_vec), 0), dim(x))
  return(result)
}

.g2 <- function(x, a, lambda) {
  result = array(pmax(lambda / apply(x - a, c(1, 2), .norm_vec), 1), dim(x)) * (x - a) + a
  return(result)
}

.f2_infty <- function(x, ga) {
  result = x * array(pmax(1 - ga / apply(x, c(1, 2), .norm_infty), 0), dim(x))
  return(result)
}

.g2_infty <- function(x, a, lambda) {
  result = array(pmax(lambda / apply(x - a, c(1, 2), .norm_infty), 1), dim(x)) * (x - a) + a
  return(result)
}

.fasjem_g <- function(a, lambda, epsilon, gamma, rho, iterMax) {
  x  = .f1(a, (4 * lambda * gamma))
  x1 = x
  x2 = x
  x3 = x
  x4 = x
  for (i in 1:iterMax) {
    p1 = .f1(x1, (4 * lambda * gamma))
    p2 = .g1(x2, a, (4 * lambda * gamma * epsilon))
    p3 = .f2(x3, lambda)
    p4 = .g2(x4, a, (epsilon * lambda))
    p  = (p1 + p2 + p3 + p4) / 4
    x1 = x1 + (p * 2 - p1 - x1) * rho
    x2 = x2 + (p * 2 - p2 - x2) * rho
    x3 = x3 + (p * 2 - p3 - x3) * rho
    x4 = x4 + (p * 2 - p4 - x4) * rho
    x  = x  + (p - x) * rho
  }
  x = .f1(x, (4 * lambda * gamma))
  return(x)
}

.fasjem_i <- function(a, lambda, epsilon, gamma, rho, iterMax) {
  x  = .f1(a, (4 * lambda * gamma))
  x1 = x
  x2 = x
  x3 = x
  x4 = x
  for (i in 1:iterMax) {
    p1 = .f1(x1, (4 * lambda * gamma))
    p2 = .g1(x2, a, (4 * lambda * gamma * epsilon))
    p3 = .f2_infty(x3, lambda)
    p4 = .g2_infty(x4, a, (epsilon * lambda))
    p  = (p1 + p2 + p3 + p4) / 4
    x1 = x1 + (p * 2 - p1 - x1) * rho
    x2 = x2 + (p * 2 - p2 - x2) * rho
    x3 = x3 + (p * 2 - p3 - x3) * rho
    x4 = x4 + (p * 2 - p4 - x4) * rho
    x  = x  + (p - x) * rho
  }
  x = .f1(x, (4 * lambda * gamma))
  return(x)
}

fasjem.EEGM <- function(covMatrix, lambda) {
  result = sign(covMatrix) * pmax(abs(covMatrix) - lambda, 0)
  result
}

fasjem.backwardMap <- function(covMatrix) {
  niuList = 0.001 * (1:1000)
  bestDet = det(fasjem.EEGM(covMatrix, 0.001))
  bestniu = 0.001
  for (i in 1:1000) {
    if (bestDet < det(fasjem.EEGM(covMatrix, niuList[i]))) {
      bestDet = det(fasjem.EEGM(covMatrix, niuList[i]))
      bestniu = niuList[i]
    }
  }
  return(solve(fasjem.EEGM(covMatrix, bestniu)))
}


#' A Fast and Scalable Joint Estimator for Learning Multiple Related Sparse
#' Gaussian Graphical Models
#'
#' The R implementation of the FASJEM method, which is introduced in the paper
#' "A Fast and Scalable Joint Estimator for Learning Multiple Related Sparse
#' Gaussian Graphical Models". Please run demo(fasjem) to learn the basic
#' functions provided by this package. For more details, please see
#' <http://proceedings.mlr.press/v54/wang17e/wang17e.pdf>.
#'
#' The FASJEM algorithm is a fast and scalable method to estimate multiple
#' related sparse Gaussian Graphical models. It solves the following equation:
#' \deqn{ \min\limits_{\Omega_{tot}} ||\Omega_{tot}||_1 + \epsilon
#' \mathcal{R}'(\Omega_{tot}) } Subject to : \deqn{ ||\Omega_{tot} -
#' inv(T_v(\hat{\Sigma}_{tot}))||_{\infty} \le \lambda_n } \deqn{
#' \mathcal{R}'^*(\Omega_{tot} - inv(T_v(\hat{\Sigma}_{tot}))) \le
#' \epsilon\lambda_n } More details are provided in the equation (3.1) of our
#' original paper.
#'
#' The \eqn{\lambda_n} in the above equation represents the hyperparameter
#' \code{lambda} who controls the sparsity level of the target precision
#' matrices.
#'
#' The \eqn{\epsilon\lambda_n} in the above equation represents the
#' regularization parameter of the second norm who controls how multiple graphs
#' share a certain pattern. Here \eqn{\epsilon} represents the input parameter
#' \code{epsilon} whose default value is 0.1.
#'
#' Other parameters in the fasjem function are described in details by the
#' Algorithm 1 in our paper.
#'
#' When \code{method = "fasjem-g"}, \eqn{\mathcal{R}'(\cdot) =
#' ||\cdot||_{\mathcal{G},2}}.
#'
#' When \code{method = "fasjem-i"}, \eqn{\mathcal{R}'(\cdot) =
#' ||\cdot||_{\mathcal{G},\infty}}.
#'
#' Please run \code{demo(fasjem)} to learn the basics.
#' For more details, please see
#' <http://proceedings.mlr.press/v54/wang17e/wang17e.pdf>.
#'
#' @param X A List of input matrices. They can be either data matrices or
#' covariance matrices. If every matrix in the X is a symmetric matrix, the
#' input matrices are assumed to be the covariance matrices from the multiple
#' related tasks.
#' @param method By using two different regularization functions as the second
#' norm in the objective, this package provides two different options for
#' regularizing the sparsity pattern shared among multiple graphs. This
#' parameter decides which function to use for the second regularization norm.
#'
#' When \code{method = "fasjem-g"}, fasjem will use the group,2 norm as the
#' second regularization function.
#'
#' When \code{method = "fasjem-i"}, fasjem will choose the group,infinity norm
#' as the second regularization function. The default value is "fasjem-g".
#' Please check the paper for more details.
#' @param lambda A positive number.  This hyperparameter controls the sparsity
#' level of the matrices. The \eqn{\lambda_n} in the following section:
#' Details.
#' @param epsilon A positive number. This hyperparameter represents the ratio
#' between the l1 norm and the second group norm. The \eqn{\epsilon} in the
#' following section: Details.
#' @param gamma A positive number. This hyperparameter is used in calculating
#' each proximity during optimization. Please check the Algorithm 1 in our
#' paper for more details.
#' @param rho A positive number. This hyperparameter controls the learning rate
#' of the proximal gradient method. Please check the Algorithm 1 in our paper
#' for more details.
#' @param iterMax An integer. The max number of iterations in the optimization
#' of fasjem.
#' @return \item{$graphs}{A list of the estimated inverse covariance matrices.}
#' @references Beilun Wang, Ji Gao, Yanjun Qi (2017). A Fast and Scalable Joint
#' Estimator for Learning Multiple Related Sparse Gaussian Graphical Models.
#' <http://proceedings.mlr.press/v54/wang17e/wang17e.pdf>
#' @keywords fasjem
#' @export
#' @details if labels are provided in the datalist as column names, result will contain labels (to be plotted)
#' @examples
#' \dontrun{
#' library(JointNets)
#' data(exampleData)
#' result = fasjem(X = exampleData, method = "fasjem-g", 0.5, 0.1, 0.1, 0.05, 10)
#' plot(result)
#' }
fasjem <-
  function(X,
           method = "fasjem-g",
           lambda = 0.5,
           epsilon = 0.1,
           gamma = 0.1,
           rho = 0.05,
           iterMax = 10) {
    if (is.data.frame(X[[1]])) {
      for (i in 1:(length(X))) {
        X[[i]] = as.matrix(X[[i]])
      }
    }

    #get number of tasks
    N = length(X)
    #get the cov/cor matrices
    if (isSymmetric(X[[1]]) == FALSE) {
      for (i in 1:N) {
        X[[i]] = stats::cov(X[[i]])
      }
    }

    tmp = array(0, dim = c(dim(X[[1]])[1], dim(X[[1]])[2], length(X)))
    for (i in 1:length(X)) {
      tmp[, , i] = X[[i]]
    }
    if (!isSymmetric(X[[1]])) {
      tmp = array(apply(tmp, 3, stats::cov), dim = c(ncol(X[[i]]), ncol(X[[i]]), length(X)))
    }

    tmp = array(apply(tmp, 3, fasjem.backwardMap), dim = c(ncol(X[[i]]), ncol(X[[i]]), length(X)))
    if (method == "fasjem-g") {
      tmp = .fasjem_g(tmp, lambda, epsilon, gamma, rho, iterMax)
    }
    if (method == "fasjem-i") {
      tmp = .fasjem_i(tmp, lambda, epsilon, gamma, rho, iterMax)
    }
    result = list()
    for (i in 1:dim(tmp)[3]) {
      result[[i]] = tmp[, , i]
    }
    result = list(graphs = result, share = NULL)
    class(result) = "fasjem"
    result = add_name_to_out(result,X)
    return(result)
  }


