library(JointNets)


readline(prompt = "Press [enter] to view simule evaluation")

simulate = simulation(n = c(100,100))
samples = simulate$samples
result = simule(
  X = samples,
  lambda = 0.1,
  epsilon = 0.45,
  covType = "cov",
  TRUE
)
evaluation(result,simulate)



readline(prompt = "Press [enter] to view jeek evaluation")


result = jeek(X = samples,
              0.3,
              covType = "cov",
              parallel = TRUE)
evaluation(result,simulate)


readline(prompt = "Press [enter] to view diffee evaluation")

result = diffee(samples[[1]], samples[[2]], 0.45)
evaluation(result,simulate)
