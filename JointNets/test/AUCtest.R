
### simule
simulationresult = simulation(n=c(100,100,100))

AUC_result = AUC_rewrite(simulationresult,
                         lambdas = seq(0.1,2,0.05)
                         ,epsilon = 2)

AUC_result
plot(AUC_result$fPM,AUC_result$tPM)

### fasjem
simulationresult = simulation(n=c(100,100,100))

AUC_result = AUC_rewrite(simulationresult,
                         gm_method = "fasjem",
                         lambdas = seq(0.1,2,0.05)
                         ,epsilon = 10)
AUC_result
plot(AUC_result$fPM,AUC_result$tPM)


### wsimule
simulationresult = simulation(n=c(100,100,100))

AUC_result = AUC_rewrite(simulationresult,
                         gm_method = "wsimule",
                         lambdas = seq(0.1,2,0.05)
                         ,epsilon = 2, W = matrix(1,20,20))
AUC_result
plot(AUC_result$fPM,AUC_result$tPM)


### jeek
### W has to be a list of matrices
m  = matrix(1,20,20)
simulationresult = simulation(n=c(100,100,100))
AUC_result = AUC_rewrite(simulationresult,
                         gm_method = "jeek",
                         W = list(m,m,m),
                         lambdas = seq(0.1,2,0.1)
                         )
AUC_result
plot(AUC_result$fPM,AUC_result$tPM)

### diffee
simulationresult = simulation(n=c(100,100))
AUC_result = AUC_rewrite(simulationresult,
                         gm_method = "diffee",
                         lambdas = seq(0.1,2,0.1)
)
AUC_result
plot(AUC_result$fPM,AUC_result$tPM)

### diffeek
simulationresult = simulation(n=c(100,100))
AUC_result = AUC_rewrite(simulationresult,
                         gm_method = "diffeek",
                         lambdas = seq(0.1,2,0.1),
                         W = list(m,m),
                         g =
)

diffeek(simulationresult$simulatedsamples[[1]],
        simulationresult$simulatedsamples[[2]],
        lambda = 1, W = matrix(0,20,20), g = c(1,2,3))

AUC_result
plot(AUC_result$fPM,AUC_result$tPM)
