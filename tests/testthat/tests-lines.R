context("lines")

context("lines arguments")
test_that("setting params works as expected",{
  expect_equal(gsplot:::graphics_params("lines", c(0,0), c(2,5)), list(x=c(0,0), y=c(2,5)))
  expect_equal(gsplot:::graphics_params("lines", x=c(0,0), y=c(2,5)), list(x=c(0,0), y=c(2,5)))
  expect_equal(gsplot:::graphics_params("lines", y=c(2,5), x=c(0,0)), list(x=c(0,0), y=c(2,5)))
})

test_that("graphics examples work", {
  
  plot(-4:4, -4:4, type = "n")  # setting up coord. system
  lines(c(0,0), c(2,5))
  lines(c(3,4,3), c(2,4,6), pch=6)
  
})

test_that("testing content of gsplot list", {
  
  gs <- gsplot(list())
  
  expect_is(gs,"gsplot")
  
  gs <- lines(gs, c(0,0), c(2,5))
  
  expect_true(gs$lines$arguments$y[1]==2)
  
  expect_false(gs$lines$arguments$y[1]==34)
  
  
  
})