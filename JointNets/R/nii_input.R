#' @export
# currently not working
convert_nii_to_datalist<-function(nii,atlas,number){
  data = nii@.Data
  mask = atlas@.Data

  AAL116mask = list()
  AAL116masksize = list()
  for (t in 1:116){
    temp <- array(0, c(61, 73, 61))
    a = 0
    for (i in 1:61){
      for (j in 1:73){
        for (k in 1:61){
          if (mask[i,j,k] == number[t]){
            temp[i,j,k] <- 1
            a = a + 1
          }
        }
      }
    }
    AAL116mask[[t]] = temp
    AAL116masksize[[t]] = a
  }

  dim = dim(data)[4]
  niidatalist = matrix(0,116, dim)
  for (a in 1: dim){
    curdata = data[,,,a]
    for (t in 1:116){
      niidatalist[t,a] = sum(AAL116mask[[t]] * curdata) / AAL116masksize[[t]]
    }
  }
}




