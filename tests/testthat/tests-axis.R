context("axis")

test_that("axis",{
  
  ## Setup up coordinate system (with x == y aspect ratio):
  plot(c(-2,3), c(-1,5), axes=FALSE)
  ## the x- and y-axis, and an integer grid
  axis(1)
  axis(2)
  axis(3)
  
})

test_that("axis gsplot",{
  gs = points(gsplot(), c(-2,3), c(-1,5)) %>% 
    axis(3)
  expect_equal(names(gs), c("view","par","axis"))
})