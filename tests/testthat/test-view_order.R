context("plot order")

test_that("plotting order within a single view is retained", {
  
  gs <- gsplot()
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    lines(c(0,3), c(2,4))
  
  expect_equal(names(gs$view),c('points','lines','window'))
})

test_that("multiple view order is retained", {
  
  gs <- gsplot()
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    lines(c(0,3), c(2,4), side=c(3,4))
  
  expect_equal(names(gs[[1]]),c('points','window'))
  expect_equal(names(gs[[2]]),c('lines','window'))
})
