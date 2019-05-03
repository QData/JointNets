#'wrapper for function JGL
#'@param X data list
#'@param lambda1 The tuning parameter for the graphical lasso penalty.
#'@param lambda2 The tuning parameter for the fused or group lasso penalty.
#'@param ... optional parameters passed to JGL() from "JGL" package
#'@return a list of estimated precision matrix
#'@export
#'@import JGL
jgl <- function(X, lambda1, lambda2, ...){
  result = JGL(Y=X,lambda1 = lambda1, lambda2 = lambda2, return.whole.theta = TRUE, ...)
  out = list(graphs = result$theta , share = NULL)
  class(out) = "jgl"
  ## fix the name issue
  out = add_name_to_out(out,X)
  return(out)
}


#data("nip_37_data")
#test = jgl(nip_37_data,1,0.1)
#plot(test)
