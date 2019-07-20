library(JointNets)
graphics.off()
par(ask = FALSE)
par(mfrow = c(1, 1))


readline(prompt = "Press [enter] to view simule evaluation")

simulationresult = simulation(n=c(100,100,100))
AUC_result = AUC(simulationresult,
                 gm_method = "simule",
                 lambdas = seq(0.1,2,0.05)
                 ,epsilon = 1)
truth = simulationresult$simulatedgraphs
result = simule(simulationresult$simulatedsamples, 0.2, 0.5, covType = "cov", TRUE)

{
cat(paste("AUC score: ", AUC_result$auc))
cat("\n")
cat("F1 score graphs: ")
cat(F1(result,truth)$graphs)
cat("\n")
cat(paste("F1 score share: ", F1(result,truth)$share))
cat("\n")
cat(paste("BIC score: ", BIC(simulationresult$simulatedsamples,result)))
plot(AUC_result$fPM,AUC_result$tPM, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC")
lines(AUC_result$fPM[order(AUC_result$fPM)], AUC_result$tPM[order(AUC_result$fPM)], xlim=range(AUC_result$fPM), ylim=range(AUC_result$tPM))
}

readline(prompt = "Press [enter] to view wsimule evaluation")

simulationresult = simulation(n=c(100,100,100))
AUC_result = AUC(simulationresult,
                 gm_method = "wsimule",
                 lambdas = seq(0.1,2,0.05)
                 ,epsilon = 1,W= matrix(1,20,20))

truth = simulationresult$simulatedgraphs
result = wsimule(simulationresult$simulatedsamples , lambda = 0.1, epsilon = 0.45, W = matrix(1,20,20), covType = "cov", TRUE)

{
  cat(paste("AUC score: ", AUC_result$auc))
  cat("\n")
  cat("F1 score graphs: ")
  cat(F1(result,truth)$graphs)
  cat("\n")
  cat(paste("F1 score share: ", F1(result,truth)$share))
  cat("\n")
  cat(paste("BIC score: ", BIC(simulationresult$simulatedsamples,result)))
  plot(AUC_result$fPM,AUC_result$tPM, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC")
  lines(AUC_result$fPM[order(AUC_result$fPM)], AUC_result$tPM[order(AUC_result$fPM)], xlim=range(AUC_result$fPM), ylim=range(AUC_result$tPM))
}


readline(prompt = "Press [enter] to view jeek evaluation")
simulationresult = simulation(n=c(100,100,100))
AUC_result = AUC(simulationresult,
                 gm_method = "jeek",
                 lambdas = seq(0.1,2,0.05)
                 , W = list(matrix(1,20,20),matrix(1,20,20),matrix(1,20,20)))

truth = simulationresult$simulatedgraphs
result = jeek(X = simulationresult$simulatedsamples,
              0.3,
              covType = "cov",
              parallel = TRUE)
{
  cat(paste("AUC score: ", AUC_result$auc))
  cat("\n")
  cat("F1 score graphs: ")
  cat(F1(result,truth)$graphs)
  cat("\n")
  cat(paste("BIC score: ", BIC(simulationresult$simulatedsamples,result)))
  plot(AUC_result$fPM,AUC_result$tPM, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC")
  lines(AUC_result$fPM[order(AUC_result$fPM)], AUC_result$tPM[order(AUC_result$fPM)], xlim=range(AUC_result$fPM), ylim=range(AUC_result$tPM))
}



readline(prompt = "Press [enter] to view diffee evaluation")
simulationresult = simulation(n=c(100,100))
AUC_result = AUC(simulationresult,
                 gm_method = "diffee",
                 lambdas = seq(0.1,2,0.05))
truth = simulationresult$simulatedgraphs
result = diffee(simulationresult$simulatedsamples[[1]], simulationresult$simulatedsamples[[2]], 0.45)

{
  cat(paste("AUC score: ", AUC_result$auc))
  cat("\n")
  cat("F1 score difference: ")
  cat(F1(result,truth)$difference)
  cat("\n")
  plot(AUC_result$fPM,AUC_result$tPM, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC")
  lines(AUC_result$fPM[order(AUC_result$fPM)], AUC_result$tPM[order(AUC_result$fPM)], xlim=range(AUC_result$fPM), ylim=range(AUC_result$tPM))
}

readline(prompt = "Press [enter] to view kdiffnet evaluation")
simulationresult = simulation(n=c(100,100))
AUC_result = AUC(simulationresult,
                 gm_method = "kdiffnet",
                 lambdas = seq(0.1,2,0.05)
                 , W = matrix(1,20,20), g = rep(0,20))
truth = simulationresult$simulatedgraphs
result = kdiffnet(exampleData[[1]], exampleData[[2]], W = matrix(1,20,20), g = rep(0,20),
                 epsilon = 0.2, lambda = 0.4,covType = "cov")
{
  cat(paste("AUC score: ", AUC_result$auc))
  cat("\n")
  cat("F1 score difference: ")
  cat(F1(result,truth)$difference)
  cat("\n")
  plot(AUC_result$fPM,AUC_result$tPM, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC")
  lines(AUC_result$fPM[order(AUC_result$fPM)], AUC_result$tPM[order(AUC_result$fPM)], xlim=range(AUC_result$fPM), ylim=range(AUC_result$tPM))
}


