context("test callouts")
test_that("lines to callouts", {
  
  expect_warning(callouts(gsplot(), c(0,3), NULL, labels=c('dogs','cats')))
  expect_warning(callouts(gsplot(), c(0,3), y=NULL, labels=c('dogs','cats')))
  
  #expect_error(callouts(gsplot(), x=c(0,3), y=1:2, angle=-30))
  #expect_error(callouts(gsplot(), x=c(0,3), y=1:2, angle='random'))
  
  callouts(gsplot(), c(0,3), y=1:2, labels=c('dogs','cats'), angle=30)
  callouts(gsplot(), c(0,3), y=1:2, labels=c('dogs','cats'), angle='auto')
  
})