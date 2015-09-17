context("view par")
test_that("par arguments", {
  
  arguments = list(x=3, y=34, col='yellow',las=3)
  expect_equal(gsplot:::par_arguments(arguments, def.funs=c(graphics::plot.xy, graphics::points.default)), list(las=3))
  expect_equal(gsplot:::formal_arguments(arguments, def.funs=c(graphics::plot.xy, graphics::points.default)), list(x=3, y=34, col='yellow'))
  
})

test_that("par within view", {
  
  gs = points(gsplot(), y=1, x=2, col="blue", pch=18, las=3)  %>% lines(2:3,4:5)  %>% points(3,4,side=c(3,4),las=1)
  expect_equal(gs[[1]]$points$col, 'blue')
  expect_equal(gs[[1]]$window$par$las, 3)
  expect_equal(gs[[2]]$points$col, 'red') # default
  expect_equal(gs[[2]]$window$par$las, 1)
  
})
