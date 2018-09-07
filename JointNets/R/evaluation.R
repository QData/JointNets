#' function to evaluate results from jointnet (from simulated data)
#' @param result result generated using the same jointnet method
#' @param simulate result generated from function simulate
#' @export
evaluation <- function(result,simulate){
  comparisonplot(result,simulate$graphs)
  cat("BIC score is", BIC(simulate$samples,result))
  cat("\n")
  cat("F1 scores are", F1(simulate$graphs ,result))
}







