context("background_color")

test_that("testing content of gsplot list for background_color", {
  
  gs <- gsplot(list())
  
  expect_is(gs,"gsplot")
  
  gs <- gsplot()  %>% 
    points(1:10, 1:10)  %>%
    lines(20:30,20:30, side=c(3,4)) %>%
    background_color(col="coral2")
  
  expect_true("background_color" %in% names(gs$view.1.2))
  
})


context("background_color arguments")
test_that("setting params works as expected",{
  expect_equal(gsplot:::function_args("gsplot","background_color", "coral2"), list(col="coral2"))
})