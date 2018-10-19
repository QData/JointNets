library(JointNets)
graphics.off()
par(ask = F)
par(mfrow = c(1, 1))

simulateresult = simulation(p =20,  n = c(100,100))
graphs = simulateresult$simulatedgraphs
layout = layout_nicely(returngraph(graphs), dim = 2)
par(mfrow = c(2, 2))
plot(graphs, type = "task", subID = 1, layout = layout)
plot(graphs, type = "task", subID = 2, layout = layout)
plot(graphs, type = "taskspecific", subID = 1, layout = layout)
plot(graphs, type = "taskspecific", subID = 2, layout = layout)


samples = simulateresult$simulatedsamples #datalist (can be used to run various algorithm)
