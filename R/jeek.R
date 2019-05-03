jeek.EEGM <- function(covMatrix, lambda){
  result = sign(covMatrix) * pmax(abs(covMatrix) - lambda, 0)
  return(result)
}

jeek.backwardMap <- function(covMatrix){
  niuList = 0.001 * (1:1000)
  bestDet = det(jeek.EEGM(covMatrix, 0.001))
  bestniu = 0.001
  for (i in 1:1000) {
    if (bestDet < det(jeek.EEGM(covMatrix, niuList[i]))) {
      bestDet = det(jeek.EEGM(covMatrix, niuList[i]))
      bestniu = niuList[i]
    }
  }
  return(solve(jeek.EEGM(covMatrix, bestniu)))
}

#A simplex solver for linear programming problem in jeek
jeek.linprogS <- function(w, b, lambda){
  # K
  # Get parameters
  K = length(b)
  con = cbind(diag(1, K, K),rep(1,K))
  # linear programming solution
  f.obj = c(w, w)
  con1 = cbind(-con, +con)
  lambda = lambda * pmin(w[1:K], w[K+1])
  b1 = lambda - b
  b2 =  lambda + b
  f.con = rbind(-diag(2 * (K + 1)), con1, -con1)
  f.dir = rep("<=", 4 * K + 2)
  f.rhs = c(rep(0, 2 * (K + 1)), b1, b2)
  lp.out = lp("min", f.obj, f.con, f.dir, f.rhs)
  beta = lp.out$solution[1:(K + 1)] - lp.out$solution[(K + 2):(2 * (K + 1))]
  if (lp.out$status == 2) warning("No feasible solution!  Try a larger tuning parameter!")
  return(beta)
}

#The parallel version for jeek
jeek.linprogSPar <- function(i, W, B, lambda){
  #get j,k
  p = dim(B)[1]
  K = dim(B)[3]
  #(1,2) (1,3) (1,4) (1,5)
  #      (2,3) (2,4) (2,5)
  #            (3,4) (3,5)
  #                  (4,5)
  #(1,2) -> (1,3) -> (2,3) -> (1,4) -> (2,4) -> (3,4)
  k = ceiling(sqrt(2 * i + 1/4) + 1/2)
  j = i - (k - 1) * (k - 2) / 2
  w = W[j,k,]
  b = B[j,k,]
  return(jeek.linprogS(w, b, lambda))
}



#' A Fast and Scalable Joint Estimator for Integrating Additional Knowledge in
#' Learning Multiple Related Sparse Gaussian Graphical Models
#'
#' A Fast and Scalable Joint Estimator for Integrating Additional Knowledge in
#' Learning Multiple Related Sparse Gaussian Graphical Models. Please run
#' demo(jeek) to learn the basic functions provided by this package.  For
#' further details, please read the original paper: Beilun Wang, Arshdeep
#' Sekhon, Yanjun Qi (2018).
#'
#' The JEEK algorithm is a novel Joint Elementary Estimator incorporating
#' additional Knowledge (JEEK) to infer multiple related sparse Gaussian
#' Graphical models from large-scale heterogeneous data. It solves the
#' following equation: \deqn{ \min\limits_{\Omega^{tot}_I, \Omega^{tot}_S}
#' ||W^{tot}_I \circ \Omega^{tot}_I||_1 + ||W^{tot}_S\circ \Omega^{tot}_S|| }
#' Subject to : \deqn{ ||W^{tot}_I \circ (\Omega^{tot} -
#' inv(T_v(\hat{\Sigma}^{tot}))) ||_{\infty} \le \lambda_n } \deqn{ ||W^{tot}_S
#' \circ (\Omega^{tot} - inv(T_v(\hat{\Sigma}^{tot}))) ||_{\infty} \le
#' \lambda_n } \deqn{ \Omega^{tot} = \Omega^{tot}_S + \Omega^{tot}_I }
#'
#' Please also see the equation (3.7) in our paper. The \eqn{\lambda_n} is the
#' hyperparameter controlling the sparsity level of the matrices and it is the
#' \code{lambda} in our function. For further details, please see our paper:
#' Beilun Wang, Arshdeep Sekhon, Yanjun Qi. A Fast and Scalable Joint Estimator
#' for Integrating Additional Knowledge in Learning Multiple Related Sparse
#' Gaussian Graphical Models. ICML 2018
#'
#' @param X A List of input matrices. They can be data matrices or
#' covariance/correlation matrices. If every matrix in the X is a symmetric
#' matrix, the matrices are assumed to be covariance/correlation matrices.
#' @param lambda A positive number. The hyperparameter controls the sparsity
#' level of the matrices. The \eqn{\lambda_n} in the following section:
#' Details.
#' @param W A list of weight matrices. The hyperparameter intergrating the
#' additional knowledge into the model. The \eqn{W_{ij}} is large means that
#' node i and node j have less probability to connect with each other. The
#' default value of each entry is 1, which means there is no additional
#' knowledge in the formulation.
#' @param covType A parameter to decide which Graphical model we choose to
#' estimate from the input data.
#'
#' If covType = "cov", it means that we estimate multiple sparse Gaussian
#' Graphical models. This option assumes that we calculate (when input X
#' represents data directly) or use (when X elements are symmetric representing
#' covariance matrices) the sample covariance matrices as input to the JEEK
#' algorithm.
#'
#' If covType = "kendall", it means that we estimate multiple nonparanormal
#' Graphical models. This option assumes that we calculate (when input X
#' represents data directly) or use (when X elements are symmetric representing
#' correlation matrices) the kendall's tau correlation matrices as input to the
#' JEEK algorithm.
#' @param intertwined indicate whether to use intertwined covariance matrix
#' @param parallel A boolean. This parameter decides if the package will use
#' the multithreading architecture or not.
#' @return \item{$graphs}{A list of the estimated inverse covariance/correlation
#' matrices.}
#' @author Beilun Wang
#' @references Beilun Wang, Arshdeep Sekhon, Yanjun Qi. A Fast and Scalable
#' Joint Estimator for Integrating Additional Knowledge in Learning Multiple
#' Related Sparse Gaussian Graphical Models. <https://arxiv.org/abs/1806.00548>
#' @export
#' @import lpSolve
#' @import parallel
#' @import pcaPP
#' @details if labels are provided in the datalist as column names, result will contain labels (to be plotted)
#' @examples
#' library(JointNets)
#' data(exampleData)
#' result = jeek(X = exampleData, 0.3, covType = "cov", parallel = FALSE)
#' plot(result)
jeek <- function(X, lambda, W = NA, covType = "cov",  intertwined = FALSE, parallel = FALSE) {
  #decide if they dataframe or not
  N = length(X)
  for (i in 1:N){
    X[[i]] = compute_cov(X[[i]],covType)
  }

  if (intertwined){
    X = intertwined(X,covType = covType)
  }

  # get key parameters
  p = dim(X[[1]])[2]
  K = length(X)
  B = array(0, dim = c(p, p, K))
  weight = array(1, dim = c(p, p, (K + 1)))
  xt = array(0, dim = c(p, p, (K + 1)))
  graphs = list()
  #transfer to 3D tensor for parallelization
  for (i in 1:length(X)) {
    B[,,i] = X[[i]]
  }
  ## add any() to fix multiple NA problems
  if (!any(is.na(W))){ ## will give warnings since W is a list
    for (i in 1:length(W)) {
      weight[,,i] = W[[i]]
    }
  }
  #Decide if X is the data matrices or cov matrices

  B = array(apply(B, 3, jeek.backwardMap), dim = c(p, p, K))

  f = function(x) jeek.linprogSPar(x, weight, B, lambda)

  if (parallel == TRUE) {
    no_cores = detectCores() - 1
    cl = makeCluster(no_cores)
    # declare variable and function names to the cluster
    clusterExport(cl, list("f", "weight", "B", "lambda", "jeek.linprogSPar", "lp", "jeek.linprogS"), envir = environment())
    numOfVariable = (p - 1) * p / 2
    result = parLapply(cl, 1:numOfVariable, f)
    #print('Done!')
    for (i in 1:numOfVariable) {
      k = ceiling(sqrt(2 * i + 1/4) + 1/2)
      j = i - (k - 1) * (k - 2) / 2
      xt[j, k, ] = result[[i]]
      xt[k, j, ] = result[[i]]
    }
    stopCluster(cl)
  }
  else{
    for(j in 1:(p-1)){
      for(k in (j+1):p){
        xt[j,k,] = jeek.linprogS(weight[j,k,], B[j,k,], lambda)
        xt[k,j,] = xt[j,k,]
      }
    }
  }
  for (i in 1:K) {
    graphs[[i]] = xt[, , i] + xt[, , (K + 1)] + diag(1,p,p)
  }
  out = list(graphs = graphs, share = NULL)
  class(out) = "jeek"
  out = add_name_to_out(out,X)
  return(out)
}


