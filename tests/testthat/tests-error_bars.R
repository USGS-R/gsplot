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

test_that("testing error_bar limits", {
  
  
  errorbarplot <- gsplot() %>% 
       points(1:7, 1:7) %>% 
       error_bar(x = 1:7, y = 1:7, y.high = 1, y.low = 1)
  
  expect_equal(ylim(errorbarplot)$side.2, c(0, 8))

  
})

test_that("testing error_bar config", {
  
  
  gsp <- gsplot(config.file = system.file("extdata", "lineScatter.yaml", package = "gsplot")) %>%
    error_bar(1:10, 1:10, y.high=1) 
  
  expect_true(gsp$global$config$config.file)
  expect_equal(gsp$view.1.2$error_bar$col, "green")
  
  gsp <- gsplot() %>%
    error_bar(1:10, 1:10, y.high=1) 
  
  expect_false(gsp$global$config$config.file)
  expect_null(gsp$view.1.2$error_bar$col)
  
})


test_that("testing default error_bar", {
  
  
  plot(1:10, 1:10)
  
  expect_silent(error_bar(5, 5, y.high=1, col="green"))

  
})