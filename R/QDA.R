QDA <- function(train,test,lambda,v, method = "diffee",...){
  mu = apply(train[[1]], 2, mean)
  #cat("mu is ", mu, "\n")
  p = nrow(train[[1]])
  delta = matrix(0,p,p)
  delta = compute_delta(train,lambda,method,...)
  values1 = apply(test[[1]],1,function(x,delta,mu) t(x - mu) %*% delta %*% (x - mu), delta = delta, mu = mu)
  values2 = apply(test[[2]],1,function(x,delta,mu) t(x - mu) %*% delta %*% (x - mu), delta = delta, mu = mu)
  right = sum(values1 > v, na.rm = TRUE) + sum(values2 < v, na.rm = TRUE)
  best_test_accuracy = right/(length(values1) + length(values1))
  return (best_test_accuracy)
}


#'graphical model model evaluation using QDA as a classifier
#'@param train a list of training data
#'@param valid a list of validation data
#'@param test a list of test data
#'@param lambda_range a vector of lambda values to train to given method, eg c(0.1,0.2,0.3)
#'@param v_seeking_length second hyperparameter length, default to 10
#'@param method name of the method to be evaluated
#'@param ... optional parameters passed to your method from JointNets package
#'@return covriance matrix / kendall tau correlation matrix
#'@export
#'@import JGL
#'@examples
#'library(JointNets)
#'data("nip_37_data")
#'split = train_valid_test_split(nip_37_data,c(0.8,0.1,0.1),10000)
#'train = split[["train"]]
#'valid = split[["valid"]]
#'test = split[["test"]]
#'v_seeking_length = 2
#'lambda_range = seq(0.5,1, length.out = 2)
#'result = QDA_eval(train,valid,test,lambda_range, v_seeking_length, method = "diffee")
#'result[["best test accuracy"]]
QDA_eval <- function(train, valid, test, lambda_range, v_seeking_length = 10,method = "diffee",...){
  if (length(train) > 2){
    stop("QDA current only support binary classification")
  }
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
    p = nrow(train[[1]])
    delta = matrix(0,p,p)

    if (method == "diffee"){
      delta = diffee(train[[1]],train[[2]],lambda,...)$graphs[[1]]
    }
    else if (method == "simule"){
      temp = simule(train,lambda,...)
      delta = temp$graphs[[1]] - temp$graphs[[2]]
    }
    else if (method == "wsimule"){
      temp = wsimule(train,lambda,...)
      delta = temp$graphs[[1]] - temp$graphs[[2]]
    }
    else if (method == "jeek"){
      temp = jeek(train,lambda = lambda,...)
      delta = temp$graphs[[1]] - temp$graphs[[2]]
    }
    else if (method == "jgl"){
      temp = jgl(train,lambda1 = lambda, lambda2 = 1, ...)
      delta = temp$graphs[[1]] - temp$graphs[[2]]
    }
    else if (method == "kdiffnet"){
      delta = kdiffnet(train[[1]],train[[2]],lambda = lambda,...)$graphs[[1]]
    }
    else {
      stop("please specify a correct method")
    }
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


  best_test_acc = QDA(train,test,best_lambda,best_v,method)

  return(list("accuracy matrix" = result_acc_diffee,"best valid accuracy" = best_acc,
              "best test accuracy" = best_test_acc,
              "best lambda" = best_lambda, "best v" = best_v))
}


compute_delta <-function(train,lambda,method = "diffee",...){
  if (method == "diffee"){
    delta = diffee(train[[1]],train[[2]],lambda,covType = "cov",...)$graphs[[1]]
  }
  else if (method == "kdiffnet"){
    delta = kdiffnet(train[[1]],train[[2]],lambda = lambda, covType = "cov",...)$graphs[[1]]
  }
  else if (method == "simule"){
    temp = simule(train,lambda,...)
    delta = temp$graphs[[1]] - temp$graphs[[2]]
  }
  else if (method == "wsimule"){
    temp = wsimule(train,lambda,...)
    delta = temp$graphs[[1]] - temp$graphs[[2]]
  }
  else if (method == "jeek"){
    temp = jeek(train,lambda = lambda,...)
    delta = temp$graphs[[1]] - temp$graphs[[2]]
  }
  else if (method == "jgl"){
    temp = jgl(train,lambda1 = lambda, lambda2 = 1, ...)
    delta = temp$graphs[[1]] - temp$graphs[[2]]
  }
  else {
    stop("please specify a correct method")
  }
}



#### commented block
if (FALSE){
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
    mean_vector = apply(training_data[[i]], 2, mean)
    precision_matrix = result$graphs[[i]]
    prob2 = -(0.5 * t(mean_vector)  %*% precision_matrix %*% mean_vector) [1,1]
    prob3 =  (t(data) %*% precision_matrix %*% mean_vector) [1,1]
    prob4 = - (0.5 * t(data) %*% precision_matrix %*% data) [1,1]

    # use inverse of precision matrix as covariance matrix?
    prob5 = - 0.5 * log(det(inv(precision_matrix)))

    prob = prob1 + prob2 + prob3 + prob4 + prob5

    if (prob > max_prob){
      max_prob = prob
      label = i
    }
  }

  return(label)
}


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

}
