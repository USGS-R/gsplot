context("test side labels")
test_that("append replace list", {
  
  gs <- gsplot() %>%
    points(1, 2, legend.name="Cool points", xlim=c(0,NA)) %>%
    lines(x=1:5, y=1:5, legend.name="Cool lines") %>%
    legend(location="topleft")
  
  expect_equal(gs$side.1$label,"")
  expect_equal(gs$side.2$label,"")
  
  gs <- lines(gs, x=1:5, y=1:5, ylab='pizza',side=c(1,4))
  expect_equal(gs$side.4$label, 'pizza')
  expect_equal(gs$side.2$label,"")
})