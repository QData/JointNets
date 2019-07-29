kdiffnet.backwardMap <-function(covMatrix, thre = "soft"){
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


kdiffnet.f1 <- function(x, ga) {
  result = sign(x) * pmax(abs(x) - ga, 0)
  result=x-result
  return(result)
}
#g1 is f3 in paper
kdiffnet.g1 <- function(x, a, lambda) {
  result = pmin(pmax(x - a, -lambda), lambda) + a
  result=x-result
  return(result)
}
# f2 is f2 in paper change this
kdiffnet.f2 <- function(x, ga,g) {
  result2 = x
  diag(result2)=0.0
  p=nrow(x)
  result=matrix(0,p,p)

  for (i in unique(g)){
    if(i!=0){
      index = which(g == i)
      result[index,index]=max((1-(ga/(norm(result2[index,index], 'F')))), 0) * result2[index,index]
    }
  }
  results=x-result
  return(results)
}

kdiffnet.g2 <- function(x, a, lambda,g) {

  B = x
  result2 = x
  p=nrow(x)
  result=matrix(0,p,p)
  diag(result2)=0.0
  for (i in unique(g)){
    if(i!=0){
      index = which(g == i)
      B[index,index]=result2[index,index]-a[index,index]
      result[index,index]=a[index,index]+(min((lambda/(norm(B[index,index], 'F'))), 1) * B[index,index])
    }
  }

  results=x-result
  return(results)
}


.proximal <- function(a, g, lambda, epsilon, gamma, rho, W, iterMax) {

  p=nrow(a)

  x0=matrix(0,p,p)


  x1 = rbind(x0,x0)
  x2=x1
  x3=x1
  x4=x1


  y1 = x1
  y2 = x2
  y3 = x3
  y4 = x4
  Id=diag(p)

  zeors=matrix(0,p,p)
  Ad=cbind(Id,zeors)
  Ag=cbind(zeors,Id)
  Atot=cbind(Id,Id)


  y=(y1+y2+y3+y4)/4

  for (i in 1:iterMax) {

    x1=Ad%*%y1
    prox1=kdiffnet.f1(x1, (4 * lambda * gamma*W))

    p1 = y1 - t(Ad)%*%prox1


    x2=Atot%*%y2
    prox2 = kdiffnet.g1(x2, a, (lambda*W))
    p2 = y2 - 2*t(Atot)%*%prox2
    x3=Ag%*%y3
    prox3 = kdiffnet.f2(x3,4*gamma*lambda*epsilon,g)
    p3 = y3 - t(Ag)%*%prox3

    x4=Atot%*%y4
    prox4 = kdiffnet.g2(x4, a, (lambda*epsilon),g)
    p4 = y4 - 2*t(Atot)%*%prox4

    p  = (p1 + p2 + p3 + p4) / 4

    y1 = y1 + (p * 2 - p1 - y) * rho
    y2 = y2 + (p * 2 - p2 - y) * rho
    y3 = y3 + (p * 2 - p3 - y) * rho
    y4 = y4 + (p * 2 - p4 - y) * rho
    y  = y  + (p - y) * rho
  }


  Delta=(Atot%*%y)
  return(Delta)
}


#' Fast and Scalable Estimator for Using Additional Knowledge in Learning
#' Sparse Structure Change of High Dimensional of Sparse Changes
#' in High-Dimensional Gaussian Graphical Models
#'
#' The kdiffnet algorithm
#'
#' @param C A input matrix for the 'control' group. It can be data matrix or
#' covariance matrix. If C is a symmetric matrix, the matrices are assumed to
#' be covariance matrix.
#' @param D A input matrix for the 'disease' group. It can be data matrix or
#' covariance matrix. If D is a symmetric matrix, the matrices are assumed to
#' be covariance matrix.
#' @param W known edge level additional knowledge. It is a square matrix of dimension p X p where p is the input dimension.
#' @param g known node level additional knowledge. It is a vector of dimension 1 X p where p is the input dimension, each entry indicating membership of node to a group, 0 for a node belonging to no group. For example, in a dataset with dimension=3,g=c(0,1,1) indicates node 1 belongs to no group, and node 2 and node 3 belong to group index 1.
#' @param epsilon A positive number. The hyperparameter controls the sparsity level of the
#' groups in g of the difference matrix
#' @param lambda A positive number. The hyperparameter controls the sparsity level of the difference matrix
#' @param knowledgeType "EV": if use overlapping node and edge level additional knowledge,"E": if only edge level additional knowledge or "V": only group level knowledge
#' @param gamma : A positive number. This hyperparameter is used in calculating each proximity during optimization
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
#'
#' @param intertwined indicate whether to use intertwined covariance matrix
#' @param thre A parameter to decide which threshold function to use for
#' \eqn{T_v}. If thre = "soft", it means that we choose soft-threshold function
#' as \eqn{T_v}. If thre = "hard", it means that we choose hard-threshold
#' function as \eqn{T_v}.
#' @param rho A positive number. This hyperparameter controls the learning rate of the proximal gradient method.
#' @param iterMax An integer. The max number of iterations in the optimization of the proximal algorithm
#' @return \item{$graphs}{A matrix of the estimated sparse changes between two
#' Gaussian Graphical Models} \item{$share}{null}
#' @author Arshdeep Sekhon
#' @export
#' @examples
#' library(JointNets)
#' data(exampleData)
#' result = kdiffnet(exampleData[[1]], exampleData[[2]],
#' W = matrix(1,20,20), g = rep(0,20),epsilon = 0.2,
#' lambda = 0.4,covType = "cov")
#' plot(result)
#' @import pcaPP
#' @importFrom stats cov
kdiffnet <- function(C,D, W, g=rep(1,100), epsilon = 1, lambda = 0.05, knowledgeType="EV",gamma=4.0,covType = "cov", intertwined=FALSE,thre = "soft",rho=0.05,iterMax=20){
  covX = compute_cov(C,covType)
  covY = compute_cov(D,covType)

  if (intertwined){
    temp = intertwined(list(covX,covY),covType = covType)
    covX = temp[[1]]
    covY = temp[[2]]
  }

  # merge diffee and kdiffnet helper methods
  backX = kdiffnet.backwardMap(covX, thre)
  backY = kdiffnet.backwardMap(covY, thre)
  B = backY -backX

  if(knowledgeType=="EV"){
    diffNet=.proximal(B, g, lambda, epsilon, gamma, rho, W, iterMax)
  }else if(knowledgeType=="V"){
    diffNet = .softThre(B, lambda)
    diag_vals = diag(diffNet)
    diag(diffNet) = 0.0
    for (i in 1:max(g)){

      index = which(g == i)
      # index, B,epsilon, lambda
      diffNet[index,index] = max(norm(B[index,index], 'F') - lambda, 0) * B[index,index] / norm(B[index,index], 'F')
    }
    diag(diffNet) = diag_vals
  }else{
    diffNet = .softThre(B, lambda*W)
  }

  diffNet = list(diffNet)
  ### share = NULL since diffee produces only the difference graph
  out = list(graphs = diffNet, share = NULL)
  class(out) = "kdiffnet"
  out = add_name_to_out(out,C)
  return(out)
}
