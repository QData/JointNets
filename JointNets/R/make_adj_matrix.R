
#'S3 method to make title for outputs from different methods
make_adj_matrix <- function(x,...) {
  UseMethod("make_adj_matrix",x)
}

make_adj_matrix.diffee <- function(x,separate = FALSE,...){

  return(make_adj_matrix_difference(x,separate))
}

make_adj_matrix.simule <- function(x,separate = FALSE,...){
  return(make_adj_matrix_joint(x,separate))
}

make_adj_matrix.wsimule <- function(x,separate = FALSE,...){
  return(make_adj_matrix_joint(x,separate))
}

make_adj_matrix.jeek <- function(x,separate = FALSE,...){
  return(make_adj_matrix_joint(x,separate))
}

make_adj_matrix.fasjem <- function(x,separate = FALSE,...){
  return(make_adj_matrix_joint(x,separate))
}

make_adj_matrix_joint <-
  function(x, separate=FALSE)
  {
    x = x$graphs
    K = length(x)
    adj = list()
    if(separate)
    {
      for(k in 1:K)
      {
        adj[[k]] = (abs(x[[k]])>1e-5)*1
      }
    }
    if(!separate)
    {
      adj = 0*x[[1]]
      for(k in 1:K)
      {
        adj = adj+(abs(x[[k]])>1e-5)*2^(k-1)
      }
    }
    return(adj)
  }

make_adj_matrix_difference<-
  function(x,separate=FALSE){
    x = x$difference
    adj = list()
    if(separate)
    {
      adj = (abs(x) > 1e-5) * 1
    }
    if(!separate)
    {
      adj = 0*x
      adj = adj+(abs(x) > 1e-5) * 1
    }
    return(adj)
  }
