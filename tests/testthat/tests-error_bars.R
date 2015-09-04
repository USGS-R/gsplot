context("error_bar")

test_that("testing content of gsplot list for multiple error bars defined", {
  
  gs <- gsplot()
  
  expect_is(gs,"gsplot")
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    error_bar(c(0,3), c(2,4), y.high=c(2,2), x.low=c(NA,1))
  
  expect_true(length(which(names(gs[['view']]) == "arrows"))==2)
  
})

test_that("testing content of gsplot list for NA given", {
  
  gs <- gsplot()
  
  expect_is(gs,"gsplot")
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    error_bar(c(0,3), c(2,4), x.low=c(NA,1))
  
  expect_equal(length(gs$view$arrows$y0), 1)
  
  expect_less_than(gs$view$arrows$x1, gs$view$arrows$x0)
  
})

test_that("testing content of gsplot list for embedded error bar", {
  
  gs <- gsplot()
  
  expect_is(gs,"gsplot")
  
  gs <- points(gs, c(0,3), c(2,4),  
               error_bar=list(x.low=c(NA,1))) 
  
  expect_true(all(names(gs[['view']]) != "error_bar"))
  
  expect_true(gs$view$arrows$x1 == 2)
  
})


