context("error_bar")

test_that("testing content of gsplot list for multiple error bars defined", {
  
  gs <- gsplot(list())
  
  expect_is(gs,"gsplot")
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    error_bar(c(0,3), c(2,4), y.high=c(2,2), x.low=c(NA,1))
  
  expect_true(length(which(names(gs) == "arrows"))==2)
  
})

test_that("testing content of gsplot list for NA given", {
  
  gs <- gsplot(list())
  
  expect_is(gs,"gsplot")
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    error_bar(c(0,3), c(2,4), x.low=c(NA,1))
  
  expect_true(length(gs$arrows$arguments$y0)==1)
  
  expect_false(gs$arrows$arguments$x1 > gs$arrows$arguments$x0)
  
})

test_that("testing content of gsplot list for embedded error bar", {
  
  gs <- gsplot(list())
  
  expect_is(gs,"gsplot")
  
  gs <- points(gs, c(0,3), c(2,4),  
               error_bar=list(x.low=c(NA,1))) 
  
  expect_true(all(names(gs) != "error_bar"))
  
  expect_true(gs$arrows$arguments$x1 == 2)
  
})


