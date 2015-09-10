context("embedding functions")

test_that("points to callouts", {
  
  gs <- points(gsplot(), c(0,3), c(2,4), callouts(labels=c('dogs','cats')))
  expect_equal(names(gs$view),c('points','callouts','window'))

})