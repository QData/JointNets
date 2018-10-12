AUC_generic <- function(a,b){
  s = 0.0
  for(i in 1 : (length(a)-1)){
    s = 1/2 * (a[i] - a[i+1]) * (b[i] + b[i+1]) + s
  }
  return(s)
}


AUC_temp <- function(simulate, method, lambda, range = 30, ...){
  tP = rep(0,range)
  fN = rep(0,range)
  fP = rep(0,range)
  tN = rep(0,range)
  fPM = rep(1, range)
  tPM = rep(1, range)
  real = simulate$graphs$share
  for (a in 1:length(simulate$graphs$graphs)){
  real = real + simulate$graphs$graphs[[a]]
  }
  real = abs(real) > 0
  result = list()
  X = simulate$samples

  if (method == "simule"){
    for(i in 2:range){
      lambda = i*0.01
      graphs = simule(X, lambda, epsilon = 1,parallel = TRUE)
      estimategrpah = graphs$share
      for (j in 1:length(graphs$graphs)){
      estimategraph = graphs$graphs[[j]] + estimategraph
      }
      testGraph = abs(estimategraph) > 0
      tP[i] = sum(testGraph & real)
      tN[i] = sum((testGraph == 0) & (real == 0))
      fP[i] = sum((testGraph == 1) & (real == 0))
      fN[i] = sum((testGraph == 0) & (real == 1))
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC(c(fPM,0),c(tPM,0))
  }

  if (method == "wsimule"){
    for(i in 2:range){
      lambda = i*0.01
      graphs = wsimule(X, lambda, epsilon = 1,parallel = TRUE)
      estimategrpah = graphs$share
      for (j in 1:length(graphs$graphs)){
        estimategraph = graphs$graphs[[j]] + estimategraph
      }
      testGraph = abs(estimategraph) > 0
      tP[i] = sum(testGraph & real)
      tN[i] = sum((testGraph == 0) & (real == 0))
      fP[i] = sum((testGraph == 1) & (real == 0))
      fN[i] = sum((testGraph == 0) & (real == 1))
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC(c(fPM,0),c(tPM,0))
  }

  if (method == "jeek"){
    for(i in 2:range){
      lambda = i*0.01
      graphs = jeek(X, lambda, parallel = TRUE)
      estimategrpah = graphs$share
      for (j in 1:length(graphs$graphs)){
        estimategraph = graphs$graphs[[j]] + estimategraph
      }
      testGraph = abs(estimategraph) > 0
      tP[i] = sum(testGraph & real)
      tN[i] = sum((testGraph == 0) & (real == 0))
      fP[i] = sum((testGraph == 1) & (real == 0))
      fN[i] = sum((testGraph == 0) & (real == 1))
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC(c(fPM,0),c(tPM,0))
  }


  if (method == "fasjem"){
    for(i in 2:range){
      lambda = i*0.01
      graphs = fasjem(X, lambda = lambda)
      estimategrpah = graphs$share
      for (j in 1:length(graphs$graphs)){
        estimategraph = graphs$graphs[[j]] + estimategraph
      }
      testGraph = abs(estimategraph) > 0
      tP[i] = sum(testGraph & real)
      tN[i] = sum((testGraph == 0) & (real == 0))
      fP[i] = sum((testGraph == 1) & (real == 0))
      fN[i] = sum((testGraph == 0) & (real == 1))
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC(c(fPM,0),c(tPM,0))
  }


  if (method == "diffee"){
    for(i in 2:range){
      lambda = i*0.01
      graphs = diffee(X[[1]], X[[2]], lambda = lambda)
      estimategrpah = graphs$graphs
      real = 0
      for (a in 1:length(simulate$graphs$graphs)){
        real = real + simulate$graphs$graphs[[a]]
      }
      real = abs(real) > 0
      testGraph = abs(estimategraph) > 0
      tP[i] = sum(testGraph & real)
      tN[i] = sum((testGraph == 0) & (real == 0))
      fP[i] = sum((testGraph == 1) & (real == 0))
      fN[i] = sum((testGraph == 0) & (real == 1))
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC(c(fPM,0),c(tPM,0))
  }


  if (method == "diffeek"){
    for(i in 2:range){
      lambda = i*0.01
      # fix the W, g interaction
      graphs = diffee(X[[1]], X[[2]], W = NULL, g = NULL, lambda = lambda)
      estimategrpah = graphs$graphs
      real = 0
      for (a in 1:length(simulate$graphs$graphs)){
        real = real + simulate$graphs$graphs[[a]]
      }
      real = abs(real) > 0
      testGraph = abs(estimategraph) > 0
      tP[i] = sum(testGraph & real)
      tN[i] = sum((testGraph == 0) & (real == 0))
      fP[i] = sum((testGraph == 1) & (real == 0))
      fN[i] = sum((testGraph == 0) & (real == 1))
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC(c(fPM,0),c(tPM,0))
  }

  result$auc = auc
  return(result)
}
#' function to return AUC score
#' @param simulate results from function simulation
#' @param method method name from any one of the jointNets methods
#' @param range values of lambda to calculate AUC score
#' @param ... parameters passed to method
#' @return a list consisting of AUC (a list of AUC scores), precision (a list of precisions)
#' and recall (a list of recalls)
#' @export
AUC <- function(simulate, method, lambdas, ...){
  length = length(lambdas)+1
  tP = rep(0,length)
  fN = rep(0,length)
  fP = rep(0,length)
  tN = rep(0,length)
  fPM = rep(1, length)
  tPM = rep(1, length)
  pres = rep(1, length)
  rec = rep(1,length)
  auc = c()
  precision = list()
  recall = list()
  result = list()

  if (method == "simule") {
    X = simulate$samples
    ### j to calcualte AUC for each estimation
    for (j in 1:length(simulate$samples)) {
      real = abs(simulate$graphs$graphs[[j]]) > 0
      for (i in 2:length) {
        graphs = simule(X, lambdas[i-1], ...)
        testGraph = abs(graphs$graphs[[j]]) > 0
        tP[i] = sum(testGraph & real)
        tN[i] = sum((testGraph == 0) & (real == 0))
        fP[i] = sum((testGraph == 1) & (real == 0))
        fN[i] = sum((testGraph == 0) & (real == 1))
        tPM[i] = tP[i] / (tP[i] + fN[i])
        fPM[i] = fP[i] / (fP[i] + tN[i])
        pres[i] = tP[i] / (tP[i] + fP[i])
        rec[i] = tP[i] / (tP[i] + fN[i])
      }
      precision[[j]] = pres
      recall[[j]] = rec
      auc[j] = AUC_generic(c(fPM, 0), c(tPM, 0))
    }

    K = length(simulate$samples)+1
    real = abs(simulate$graphs$share) > 0
    for (i in 2:length) {
      graphs = simule(X, lambdas[i-1], ...)
      testGraph = abs(graphs$share) > 0
      tP[i] = sum(testGraph & real)
      tN[i] = sum((testGraph == 0) & (real == 0))
      fP[i] = sum((testGraph == 1) & (real == 0))
      fN[i] = sum((testGraph == 0) & (real == 1))
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
      pres[i] = tP[i] / (tP[i] + fP[i])
      rec[i] = tP[i] / (tP[i] + fN[i])
    }
    precision[[K]] = pres
    recall[[K]] = rec
    auc[K] = AUC_generic(c(fPM, 0), c(tPM, 0))
  }



  if (method == "wsimule"){
    X = simulate$samples
    ### j to calcualte AUC for each estimation
    for (j in 1:length(simulate$samples)) {
      real = abs(simulate$graphs$graphs[[j]]) > 0
      for (i in 2:length) {
        graphs = wsimule(X, lambdas[i-1], ...)
        testGraph = abs(graphs$graphs[[j]]) > 0
        tP[i] = sum(testGraph & real)
        tN[i] = sum((testGraph == 0) & (real == 0))
        fP[i] = sum((testGraph == 1) & (real == 0))
        fN[i] = sum((testGraph == 0) & (real == 1))
        tPM[i] = tP[i] / (tP[i] + fN[i])
        fPM[i] = fP[i] / (fP[i] + tN[i])
        pres[i] = tP[i] / (tP[i] + fP[i])
        rec[i] = tP[i] / (tP[i] + fN[i])
      }
      precision[[j]] = pres
      recall[[j]] = rec
      auc[j] = AUC_generic(c(fPM, 0), c(tPM, 0))
    }

    K = length(simulate$samples)+1
    real = abs(simulate$graphs$share) > 0
    for (i in 2:length) {
      graphs = simule(X, lambdas[i-1], ...)
      testGraph = abs(graphs$share) > 0
      tP[i] = sum(testGraph & real)
      tN[i] = sum((testGraph == 0) & (real == 0))
      fP[i] = sum((testGraph == 1) & (real == 0))
      fN[i] = sum((testGraph == 0) & (real == 1))
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
      pres[i] = tP[i] / (tP[i] + fP[i])
      rec[i] = tP[i] / (tP[i] + fN[i])
    }
    precision[[K]] = pres
    recall[[K]] = rec
    auc[K] = AUC_generic(c(fPM, 0), c(tPM, 0))
  }



  if (method == "fasjem"){
  }

  if (method == "jeek"){
  }

  if (method == "diffee"){
  }

  if (method == "diffeek"){

  }

  result$auc = auc
  result$precision = precision
  result$recall = recall
  return(result)
}

experiment_auc <- function(input1, input2, method = "fastDiff-1", trueDelta, range = 30){
  tP = rep(0,range)
  fN = rep(0,range)
  fP = rep(0,range)
  tN = rep(0,range)
  fPM = rep(1, range)
  tPM = rep(1, range)
  real = abs(trueDelta) > 0
  result = list()

  if (method == "fastDiff-1"){
    backX = .backwardMap(input1)
    backY = .backwardMap(input2)
    backDelta = backX %*% (input1 - input2) %*% backY
    for(i in 2:range){
      lambda = i*0.01
      estimateDelta = .softThre(backDelta, lambda)
      testGraph = abs(estimateDelta) > 0
      tP[i] = sum(testGraph & real)
      tN[i] = sum((testGraph == 0) & (real == 0))
      fP[i] = sum((testGraph == 1) & (real == 0))
      fN[i] = sum((testGraph == 0) & (real == 1))
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC(c(fPM,0),c(tPM,0))
  }

  if (method == "fastDiff-2"){
    backX = .backwardMap(input1)
    backY = .backwardMap(input2)
    backDelta = backX - backY
    for(i in 2:range){
      lambda = i*0.01
      estimateDelta = .softThre(backDelta, lambda)
      testGraph = abs(estimateDelta) > 0
      tP[i] = sum(testGraph & real)
      tN[i] = sum((testGraph == 0) & (real == 0))
      fP[i] = sum((testGraph == 1) & (real == 0))
      fN[i] = sum((testGraph == 0) & (real == 1))
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC(c(fPM,0),c(tPM,0))
  }

  if (method == "flasso"){
    X = list(input1, input2)
    for(i in 2:range){
      lambda = i*0.01
      graphs_fused_0.05 = JGL(X,penalty = "fused",0.05,lambda,return.whole.theta = TRUE)$theta
      estimateDelta = graphs_fused_0.05[[1]] - graphs_fused_0.05[[2]]
      testGraph = abs(estimateDelta) > 0
      tP[i] = sum(testGraph & real)
      tN[i] = sum((testGraph == 0) & (real == 0))
      fP[i] = sum((testGraph == 1) & (real == 0))
      fN[i] = sum((testGraph == 0) & (real == 1))
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC(c(fPM,0),c(tPM,0))
  }

  if (method == "dpm"){
    test = dpm(input1, input2, nlambda=range ,tuning="aic")
    print(test$lambda)
    for(i in 1:range){
      estimateGraph = test$dpm[[(range+1-i)]]
      testGraph = abs(estimateGraph) > 0
      tP[i] = sum(testGraph & real)
      tN[i] = sum((testGraph == 0) & (real == 0))
      fP[i] = sum((testGraph == 1) & (real == 0))
      fN[i] = sum((testGraph == 0) & (real == 1))
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC(c(fPM,0),c(tPM,0))
  }


  if (method == "simule"){
    X = list(input1, input2)
    for(i in 2:range){
      lambda = i*0.01
      graphs = simule(X, lambda, epsilon = 1,parallel = TRUE)
      estimateDelta = graphs[[1]] - graphs[[2]]
      testGraph = abs(estimateDelta) > 0
      tP[i] = sum(testGraph & real)
      tN[i] = sum((testGraph == 0) & (real == 0))
      fP[i] = sum((testGraph == 1) & (real == 0))
      fN[i] = sum((testGraph == 0) & (real == 1))
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC(c(fPM,0),c(tPM,0))
  }

  result$auc = auc
  result$fPM = c(fPM,0)
  result$tPM = c(tPM,0)
  return(result)
}
