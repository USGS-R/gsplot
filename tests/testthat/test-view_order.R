context("plot order")

test_that("plotting order within a single view is retained", {
  
  gs <- gsplot()
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    lines(c(0,3), c(2,4))
  
  expect_equal(names(gs$view),c('par','points','lines'))
})

test_that("multiple view order is retained", {
  
  gs <- gsplot()
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    lines(c(0,3), c(2,4), side=c(3,4))
  views <- gsplot:::views(gs)
  expect_equal(names(views[[1]]),c('par','points'))
  expect_equal(names(views[[2]]),c('par','lines'))
})
