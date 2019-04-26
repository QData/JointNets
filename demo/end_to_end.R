library(JointNets)
graphics.off()
par(ask = FALSE)
par(mfrow = c(1, 1))

readline(prompt = "Press [enter] to view diffee evaluation")

## (simulation) simulate samples of two groups
simulationresult = simulation(n=c(100,100))
AUC_result = AUC(simulationresult,
                 gm_method = "diffee",
                 lambdas = seq(0.1,2,0.05))
truth = simulationresult$simulatedgraphs

## (learning) compute results for diffee
result = diffee(simulationresult$simulatedsamples[[1]], simulationresult$simulatedsamples[[2]], 0.45)


## (evaluation) evaluate diffee performance
{
  cat(paste("AUC score: ", AUC_result$auc))
  cat("\n")
  cat("F1 score difference: ")
  cat(F1(result,truth)$difference)
  cat("\n")
  plot(AUC_result$fPM,AUC_result$tPM, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC")
  lines(AUC_result$fPM[order(AUC_result$fPM)], AUC_result$tPM[order(AUC_result$fPM)], xlim=range(AUC_result$fPM), ylim=range(AUC_result$tPM))
}


## (application) classification using QDA
split = train_valid_test_split(simulationresult$simulatedsamples,c(0.6,0.2,0.2),1000)
train = split["train"]
valid = split["valid"]
test = split["test"]

v_seeking_length = 200
lambda_range = seq(0.1,0.3,length.out = 50)
result = QDA_eval(train,valid,test,lambda_range, v_seeking_length, method = "diffee")

result["best test accuracy"]
