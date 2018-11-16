

if(FALSE){
fname = system.file(
  file.path("nifti", "mniRL.nii.gz"),
  package = "oro.nifti")
eve = neurobase::readnii(fname)

setwd("/Users/ouchouyang/Desktop")
data = neurobase::readnii("OHSU_0050147_func_minimal.nii")
data = data@.Data
atlas = neurobase::readnii("aal_roi_atlas.nii")
atlas = atlas@.Data
}

make_binary_atlas<-function(atlas){
  areas = sort(unique(c(atlas)))[-1] ## get rid of 0
  num_areas = length(areas)
  binary_atlas = array(dim = c(dim(atlas),num_areas))
  for (i in 1:num_areas){
    binary_atlas[,,,i] = (atlas/areas[i] == 1)*1
  }
  return(binary_atlas)
}

#' convert nii data to datalist according to an atlas mask
#' @param data data from fmri scan (4d array)
#' @param atlas atlas of corresponding size (3d array)
#' @export
#' @details need to test extensively on this
convert_nii_to_datalist<-function(data,atlas){
  binary_atlas = make_binary_atlas(atlas)
  num_areas = dim(binary_atlas)[4]
  count_areas = c()
  for (i in 1:num_areas){
  count_areas[i] = sum(binary_atlas[,,,i] == 1)
  }
  num_slices = dim(data)[4]
  datalist = array(dim = c(num_areas,num_slices))
  for (i in 1:num_slices){
    for (j in 1:num_areas){
      datalist[j,i] = sum(data[,,,i] * binary_atlas[,,,j]) / count_areas[j]
    }
  }
  return(datalist)
}





