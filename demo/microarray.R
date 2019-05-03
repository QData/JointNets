library(JointNets)

graphics.off()
par(ask = FALSE)
par(mfrow = c(1, 1))

devtools::install_github('ramhiser/datamicroarray')
library(datamicroarray)
# n = 217, p = 1413, K = 3
data('christensen', package = 'datamicroarray')
# n = 168, p = 2905, K = 2
#data('gravier', package = 'datamicroarray')
# n = 102, p = 5565, K = 4
#data('su', package = 'datamicroarray')

datamatrix = as.matrix(christensen[[1]])
datalist = rep(list(NULL),3)
for (i in 1:nrow(datamatrix)){
  if(christensen[[2]][i] == "other"){
    datalist[[1]] = rbind(datalist[[1]],datamatrix[i,])
  }
  if(christensen[[2]][i] == "blood"){
    datalist[[2]] = rbind(datalist[[2]],datamatrix[i,])
  }
  if(christensen[[2]][i] == "placenta"){
    datalist[[3]] = rbind(datalist[[3]],datamatrix[i,])
  }
}

colnames(datalist[[1]]) = colnames(datamatrix)
colnames(datalist[[2]]) = colnames(datamatrix)
colnames(datalist[[3]]) = colnames(datamatrix)

## reduce dimension of the data
datalist = dimension_reduce(datalist)
result = simule(datalist,0.3,1,covType = "kendall")
label = colnames(datalist[[1]])
graph = returngraph(result)
layout = layout_nicely(graph, dim = 2)

readline(prompt = "Press [enter] to display four plots showing all graphs, shared graph, task specific 1 and task specific 2")


{
  par(mfrow = c(2, 2))
  plot(result, type = "task", layout = layout)
  plot(result, type = "share", layout = layout)
  plot(result,
       type = "taskspecific",
       subID = 1,
       layout = layout)
  plot(result,
       type = "taskspecific",
       subID = 2,
       layout = layout)
}
