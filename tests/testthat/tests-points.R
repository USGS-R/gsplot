context("points")

test_that("graphics examples work", {
  
  plot(-4:4, -4:4, type = "n")  # setting up coord. system
  points(rnorm(200), rnorm(200), col = "red")
  
  dev.off()
  plot(-4:4, -4:4, type = "n", xlim=c(0,100000), ylim=c(0,1))  # setting up coord. system
  lx <- seq(1, 5, length = 41)
  xy = xy.coords(x=10^lx,y=exp(-.5*lx^2))
  plot.xy(xy, type='p')

})

test_that("testing content of gsplot list", {
  
  gs <- gsplot(list())
  
  expect_is(gs,"gsplot")
  
  gs <- points(gs, y=1, x=2, col="blue", pch=18)
  
  expect_true(gs$points$arguments$x == 2)
  
  expect_false(gs$points$arguments$col=="green")
  
  expect_equal(gs$points$arguments$pch,18)
    
  
})

