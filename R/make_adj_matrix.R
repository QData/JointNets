## method to make adjancey matrix for outputs from different methods
## @export
## @param x result from JointNet methods
## @param separate determines whther the adj matrix is accumulative or individual
## @return a single adj matrix or a list of adj matrices
make_adj_matrix <- function(x, separate = FALSE) {
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
      adj = adj+(abs(x[[k]])>1e-5)*2^(k-1) ## why 2^(k-1)?
      #adj = adj+(abs(x[[k]])>1e-5)*1
    }
  }
  return(adj)
}
