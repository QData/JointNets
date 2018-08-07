setwd("C:/Users/Karen Chen/Desktop")
niidata = neurobase::readnii("Caltech_0051456_func_preproc.nii.gz")
mask = neurobase::readnii("raal_mask_pad.nii")
number = as.matrix(read.csv("aal_labels.csv", header = FALSE))[,1]
number = number[-1:-2]
number = as.numeric(number)
datalist = convert_nii_to_datalist(niidata,mask,number)

library(JointNets)

graphics.off()
par(ask=F)
par(mfrow=c(1,1))

data(aal116coordinates)
layout = cbind(aal116coordinates$x.mni + 90,aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
result = simule(datalist, 0.2, 1, covType = "cov", TRUE)

### obtain graph for creating layout
#graph = returngraph.simule(result)

### create a fixed layout on cancergraph for plotting
#braintemplate <- neurobase::readnii(system.file("MNI152_T1_1mm_brain.nii.gz", package = "brainR"),reorient = FALSE)

color = as.integer(aal116coordinates$lobe)
plotbrain(result, type = "task", neighbouroption = "task",
          subID = NULL, index = NULL, layout = layout)

plotbrain(result, type = "share", neighbouroption = "task",
          subID = NULL, index = NULL, layout = layout)

plotbrain(result, type = "taskspecific", neighbouroption = "task",
          subID = 1, index = NULL, layout = layout)

plotbrain(result, type = "taskspecific", neighbouroption = "task",
          subID = 2, index = NULL, layout = layout)
