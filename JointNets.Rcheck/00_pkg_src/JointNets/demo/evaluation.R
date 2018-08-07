simulate = simulateGraph()
simulationsamples = generateSampleList(simulate, c(100, 100))
result = simule(
  X = simulationsamples,
  lambda = 0.1,
  epsilon = 0.45,
  covType = "cov",
  TRUE
)
comparisonplot(simulate,result)

cat(paste("BIC score is",BIC(result$graphs,100,20)))

cat(paste("F1 score is", F1(simulate,result)))
