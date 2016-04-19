context("modify_side")

test_that("modify side does nothing", {
  gs <- gsplot()
  gs <- lines(gs, c(1,10), c(1,10))
  gs <- gsplot:::modify_side(gs, side=c(1,2), xlim=c(0,12))
  expect_equal(gs$side.1$lim, c(1,10))
})