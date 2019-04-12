QDA_diffee_eval <- function(train, valid, lambda_range,v_seeking_length){
  result_acc_diffee = matrix(0,v_seeking_length,length(lambda_range))
  #print(dim(result_acc_diffee))
  i = 0
  j = 0
  best_acc = 0
  best_lambda = 0
  best_v = 0
  for (lambda in lambda_range){
    #cat("Starting with lambda ", lambda, "\n")
    i = i + 1
    mu = apply(train[[1]], 2, mean)
    #cat("mu is ", mu, "\n")
    delta = diffee(train[[1]],train[[2]],lambda,covType = "cov")$graphs[[1]]
    #cat("delta is ", delta, "\n")
    values1 = apply(valid[[1]],1,function(x,delta,mu) t(x - mu) %*% delta %*% (x - mu), delta = delta, mu = mu)
    values2 = apply(valid[[2]],1,function(x,delta,mu) t(x - mu) %*% delta %*% (x - mu), delta = delta, mu = mu)
    #cat("values1 are ", values1, "\n")
    min_v = min(values1,values2)
    max_v = max(values1,values2)
    #cat("min max v are ", min_v, " ", max_v, "\n")


    #mu = apply(nip_37_data[[1]], 2, mean)
    #delta = diffee(nip_37_data[[1]],nip_37_data[[2]],0.01,covType = "kendall")$graph[[1]]
    #temp1 = apply(nip_37_data[[1]],1,function(x,delta,mu) t(x - mu) %*% as.matrix(delta) %*% (x - mu), delta = delta, mu = mu)
    #temp2 = apply(nip_37_data[[2]],1,function(x,delta,mu) t(x - mu) %*% as.matrix(delta) %*% (x - mu), delta = delta, mu = mu)


    for (v in seq(min_v, max_v, length.out = v_seeking_length)){
      j = j + 1
      right = sum(values1 > v, na.rm = TRUE) + sum(values2 < v, na.rm = TRUE)
      #print(c(i,j))
      result_acc_diffee[j, i] = right/(length(values1) + length(values1))
      if (result_acc_diffee[j, i] > best_acc){
        best_acc = result_acc_diffee[j, i]
        best_lambda = lambda
        best_v = v
      }
      #print(result_acc_diffee[j, i])
    }
    j = 0
  }
  return(list("accuracy matrix" = result_acc_diffee,"best accuracy" = best_acc,
              "best lambda" = best_lambda, "best v" = best_v))
}


QDA <- function(result, data, training_data){
  num_graphs = length(result$graphs)
  label = -1
  max_prob = -10000000000;
  num_training_data = 0
  for (i in 1:num_graphs){
    num_training_data = num_training_data + dim(training_data[[i]])[1]
  }

  for (i in 1:num_graphs){
    prob1 = log(dim(training_data[[i]])[1]/num_training_data)
    prob2 = - (0.5 * t(apply(training_data[[i]], 2, mean))  %*% result$graphs[[i]] %*% apply(training_data[[i]], 2, mean))[1,1]
    prob3 =  (t(data) %*% result$graphs[[i]] %*% apply(training_data[[i]], 2, mean))[1,1]
    prob4 = - (0.5 * t(data) %*% result$graphs[[i]] %*% data)[1,1]
    prob5 = 0
    if (det(cov(training_data[[i]])) > 0){
      prob5 = - 0.5 * log(det(cov(training_data[[i]])))
    }

    prob = prob1 + prob2 + prob3 + prob4 + prob5
    #print(prob)
    if (prob > max_prob){
      max_prob = prob
      label = i
    }
  }
  return(label)
}

#QDA(testgraph,testing[[1]][1,],training)
### evaluate accuracy rate for data_list
evaluate_QDA <- function(result, data_list, training_data){
  num_data = 0
  num_wrong = 0
  for (i in 1:length(data_list)){
    num_data = num_data + dim(data_list[[i]])[1]
  }
  for (i in 1:length(data_list)){
    for (j in 1:dim(data_list[[i]])[1]){
      #print(paste0("start with label ", i, " data index ", j))
      if(QDA(result, data_list[[i]][j,], training_data) != i){
        num_wrong = num_wrong + 1
      }
    }
  }
  accuracy_rate = 1 - num_wrong/num_data
  return(accuracy_rate)
}

