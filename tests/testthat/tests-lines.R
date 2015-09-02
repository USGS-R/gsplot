
context("lines arguments")
test_that("setting params works as expected",{
  expect_equal(gsplot:::function_args("graphics","lines", c(0,0), c(2,5)), list(x=c(0,0), y=c(2,5)))
  expect_equal(gsplot:::function_args("graphics","lines", x=c(0,0), y=c(2,5)), list(x=c(0,0), y=c(2,5)))
  expect_equal(gsplot:::function_args("graphics","lines", y=c(2,5), x=c(0,0)), list(x=c(0,0), y=c(2,5)))
})


context("lines")

test_that("graphics examples work", {
  
  plot(-4:4, -4:4, type = "n")  # setting up coord. system
  lines(c(0,0), c(2,5))
  lines(c(3,4,3), c(2,4,6), pch=6)
})

test_that("testing content of gsplot list", {
  
   gs <- gsplot()
   expect_is(gs,"gsplot")
   
   gs <- lines(gs, c(0,0), c(2,5))
 
   expect_equal(gs$view$lines$y[1], 2)
   
   expect_less_than(gs$view$lines$y[1], 34)
  
})