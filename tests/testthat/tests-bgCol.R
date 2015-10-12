context("bgCol")

test_that("testing content of gsplot list for bgCol", {
  
  gs <- gsplot(list())
  
  expect_is(gs,"gsplot")
  
  gs <- gsplot()  %>% 
    points(1:10, col="black")  %>% 
    bgCol(col="coral2")
  
  expect_true(any(names(gs) %in% "bgCol"))
  
})


context("bgCol arguments")
test_that("setting params works as expected",{
  expect_equal(gsplot:::function_args("gsplot","bgCol", "coral2"), list(col="coral2"))
})