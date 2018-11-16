AUC <- function(a,b){
  s = 0.0

  for(i in 1 : (length(a)-1)){
    s = 1/2 * (a[i] - a[i+1]) * (b[i] + b[i+1]) + s
  }
  s
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
