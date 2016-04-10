context("test callouts")
test_that("lines to callouts", {
  
  g = callouts(gsplot(), c(0,3), NULL, labels=c('dogs','cats'))
  expect_error(print(g))
  callouts(gsplot(), c(0,3), y=1:2, labels=c('dogs','cats'), angle=30)
  callouts(gsplot(), c(0,3), y=1:2, labels=c('dogs','cats'), angle='auto')
  
})
