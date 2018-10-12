### test to add legend and title
### try to get rid of "press to continue"
{
  open3d()
  cols <- rainbow(7)
  layout3d(matrix(1:16, 4,4), heights=c(1,3,1,3))
  text3d(0,0,0,"tetrahedron3d"); next3d()
  shade3d(tetrahedron3d(col=cols[1])); next3d()
  legend3d(
    "topright" ,
    legend = "hello"
  )
}


{
  open3d()
  points3d(rnorm(10), rnorm(10), rnorm(10))

  # First add standard axes
  axes3d()

  # and one in the middle (the NA will be ignored, a number would
  # do as well)
  axis3d('x', pos = c(NA, 0, 0))

  # add titles
  title3d('main', 'sub', 'xlab', 'ylab', 'zlab')

  rgl.bringtotop()
}

{
  with(iris, plot3d(Sepal.Length, Sepal.Width, Petal.Length,
                    type="s", col=as.numeric(Species)))
  legend3d(
    "topright" ,
    legend = "hello"
  )
}
