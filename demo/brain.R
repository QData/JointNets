library(JointNets)

graphics.off()
par(ask=FALSE)
par(mfrow=c(1,1))

data(ABIDE_aal116_timeseries)
data(aal116coordinates)
layout = cbind(aal116coordinates$x.mni + 90,aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
result = simule(ABIDE_aal116_timeseries, 0.2, 1, covType = "cov", TRUE)

plot_brain(result, type = "task", neighbouroption = "task",
                         subID = NULL, index = NULL, layout = layout)

open3d()
plot_brain(result, type = "task", neighbouroption = "task",
          subID = 1, index = NULL, layout = layout)


