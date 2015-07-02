context("lines")

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