library(JointNets)
graphics.off()
par(ask = F)
par(mfrow = c(1, 1))

simulateresult = simulation(p = 20,  n = c(100,100))
truth = simulateresult$simulatedgraphs
layout = layout_nicely(returngraph(truth), dim = 2)
result = simule(simulateresult$simulatedsamples, 0.2, 0.5, covType = "cov", TRUE) # simule

par(mfrow = c(1, 2))
plot(truth, type = "task", layout = layout)
plot(result, type = "task", layout = layout)

(abs(truth$share)>1e-5)*1 == (abs(result$share)>1e-5)*1
F1_single(  (abs(truth$share)>1e-5)*1  ,  (abs(result$share)>1e-5)*1  ) ## NaN accuracy too low

F1score = F1(result,truth) #F1score is the result

# jeek (no share)
result = jeek(simulateresult$simulatedsamples,
              0.25,
              covType = "kendall",
              parallel = TRUE)

F1score = F1(result,truth)


# diffee (difference)
result = diffee(simulateresult$simulatedsamples[[1]], simulateresult$simulatedsamples[[2]], 0.1)
F1score = F1(result,truth)


# sanity check (not run)
{
m1 <- round(matrix(runif(10*10), 10, 10))
m2 <- round(matrix(runif(10*10), 10, 10))
m3 <- round(matrix(runif(10*10), 10, 10))
testtruth = list(graphs = list(m1,m2),share = m3)
F1_single(m2,m2)
}


### BIC score
result1 = simule(simulateresult$simulatedsamples, 0.2, 0.5, covType = "cov", TRUE) # simule
result2 = jeek(simulateresult$simulatedsamples,
              0.25,
              covType = "kendall",
              parallel = TRUE)
result3 = diffee(simulateresult$simulatedsamples[[1]], simulateresult$simulatedsamples[[2]], 0.1)
BIC(simulateresult$simulatedsamples,result1)
BIC(simulateresult$simulatedsamples,result2)

##BIC(simulateresult$simulatedsamples,result3)
## not working on diffee
