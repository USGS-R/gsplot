context("views_with_side")
test_that("views with side retains order", {
  
  usrDef <- gsplot(mar=c(4,4,4,4), xaxs='r', yaxs='r') %>% 
    points(x=1, y=2, side=c(3,2), legend.name="Points 1", cex=3, xlab='cat') %>% 
    points(4,3,side=1,2)
  expect_error(gsplot:::views_with_side(usrDef,c(1,3)))
  expect_equal(gsplot:::views_with_side(usrDef,3), 1)
  expect_equal(gsplot:::views_with_side(usrDef,1), 2)
  
})