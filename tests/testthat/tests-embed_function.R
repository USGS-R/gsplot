context("embedding functions")

test_that("points to callouts", {
  
  gs <- gsplot()
  

  gs <- points(gs, c(0,3), c(2,4), callouts(labels=c('dogs','cats')))

})