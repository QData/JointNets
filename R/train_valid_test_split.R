#' split a datalist to train,validation and test
#' @param datalist a datalist
#' @param ratio ratio of the split (train, validation and test), eg, c(0.8,0.1,0.1)
#' @param seed seed number
#' @return a list of train,validation and test datalist
#' @export
#' @examples
#' library(JointNets)
#' data("nip_37_data")
#’ split = train_valid_test_split(nip_37_data,c(0.8,0.1,0.1),10000)
train_valid_test_split <- function(datalist, ratio, seed){
  set.seed(seed)
  num_context = length(datalist)
  train_list = rep(list(0),num_context)
  valid_list = rep(list(0),num_context)
  test_list = rep(list(0),num_context)
  for (i in 1: num_context){
    assignment = sample(1:3, size = nrow(datalist[[i]]),
                        prob = ratio, replace = TRUE)
    train = datalist[[i]][assignment == 1, ]
    validation = datalist[[i]][assignment == 2, ]
    test = datalist[[i]][assignment == 3, ]
    train_list[[i]] = train
    valid_list[[i]] = validation
    test_list[[i]] = test
  }
  return(list("train" = train_list, "valid" = valid_list, "test" = test_list))
}
