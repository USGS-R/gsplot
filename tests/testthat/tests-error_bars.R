context("error_bar")

test_that("testing content of gsplot list for multiple error bars defined", {
  
  gs <- gsplot()
  
  expect_is(gs,"gsplot")
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    error_bar(c(0,3), c(2,4), y.high=c(2,2), x.low=c(NA,1))
  
  expect_true(length(which(names(gs[['view.1.2']]) == "arrows"))==2)
  
})

test_that("testing content of gsplot list for NA given", {
  
  gs <- gsplot()
  
  expect_is(gs,"gsplot")
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    error_bar(c(0,3), c(2,4), x.low=c(NA,1))
  
  expect_equal(length(gs$view$arrows$y0), 1)
  
  expect_lt(gs$view$arrows$x1, gs$view$arrows$x0)
  
})
test_that("testing content of gsplot list for NA given", {
  gs <- gsplot()
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    error_bar(c(0,3), c(2,4), x.high=c(NA,1))
  
  expect_gt(gs$view$arrows$x1, gs$view$arrows$x0)
})
test_that("testing content of gsplot list for embedded error bar", {
  
  
  gs <- points(gsplot(), c(0,3), c(2,4),  
               error_bar(x.low=c(NA,1))) 
  
  expect_true(all(names(gs[['view']]) != "error_bar"))
  
  expect_true(gs$view$arrows$x1 == 2)
  
})

test_that("warning for calling default method", {
  expect_warning(error_bar(c(0,3), c(2,4), x.low=c(NA,1)))
})  


