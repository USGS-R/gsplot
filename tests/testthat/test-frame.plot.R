context("frame.plot")

test_that("frame.plot default", {
  gs <- gsplot()
  expect_true(!is.null(gs$global$config$frame.plot))
  
})
test_that("frame.plot arguments work as expected", {
  gs <- gsplot() %>% points(1,2)
  
  expect_true(gs$global$config$frame.plot)
  
  gs <- lines(gs, 1,2, frame.plot=FALSE)
  expect_false(gs$global$config$frame.plot)
  gs <- lines(gs, 1,2, frame.plot=TRUE)
  expect_true(gs$global$config$frame.plot)
})

