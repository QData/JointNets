#'wrapper for function JGL fromo package "JGL"
#'@param X data list
#'@param lambda1 The tuning parameter for the graphical lasso penalty.
#'@param lambda2 The tuning parameter for the fused or group lasso penalty.
#'@param ... optional parameters passed to JGL() from "JGL" package
#'@return a list of estimated precision matrix
#'@export
#'@import JGL
#'@examples
#' library(JointNets)
#' data(exampleData)
#' result = jgl(exampleData,0.1,0.01)
#' plot(result)
jgl <- function(X, lambda1, lambda2, ...){
  result = JGL(Y=X,lambda1 = lambda1, lambda2 = lambda2, return.whole.theta = TRUE, ...)
  out = list(graphs = result$theta , share = NULL)
  class(out) = "jgl"
  ## fix the name issue
  out = add_name_to_out(out,X)
  return(out)
}
