context("error_bar")

test_that("testing content of gsplot list for multiple error bars defined", {
  
  gs <- gsplot(xaxs="r", yaxs="r")
  
  expect_is(gs,"gsplot")
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    error_bar(x=c(0,3), y=c(2,4), y.high=c(2,2), x.low=c(NA,1))
  
  expect_true(all(which(names(gs[['view.1.2']]) == "error_bar") %in% c(3,4)))
  
})

test_that("testing content of gsplot list for NA given", {
  
  gs <- gsplot()
  
  expect_is(gs,"gsplot")
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    error_bar(c(0,3), c(2,4), x.low=c(NA,1))
  
  expect_equal(length(gs$view.1.2$error_bar$y0), 0)
  
  expect_true(is.na(gs$view.1.2$error_bar$x.low[1]))
  
})
test_that("testing content of gsplot list for NA given", {
  gs <- gsplot()
  
  gs <- points(gs, c(0,3), c(2,4)) %>% 
    error_bar(c(0,3), c(2,4), x.high=c(NA,1))
  
  expect_true(is.na(gs$view.1.2$error_bar$x.high[1]))
})
test_that("testing content of gsplot list for embedded error bar", {
  
  
  gs <- points(gsplot(), c(0,3), c(2,4),  
               error_bar(x.low=c(NA,1))) 
  
  expect_true("error_bar" %in% names(gs[['view.1.2']]))
  
  expect_true(all.equal(gs$view.1.2$error_bar$x , c(0,3)))
  
})



