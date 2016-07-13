context("legend applies proper args")

test_that("legend user args", {
  usrDef <- gsplot(mar=c(4,4,4,4), xaxs='r', yaxs='r') %>% 
    points(x=1, y=2, side=c(3,2), legend.name="Points 1", cex=3, xlab='cat', col='dogerblue') %>% 
    lines(x=c(3,4,3), y=c(2,4,6), legend.name="Lines 1", lty=5, col="orange") %>% 
    legend()
  expect_equal(usrDef$legend$legend.auto$col, c('dogerblue','orange'))
})

test_that("legend specified before legend.name args", {
  gs <- gsplot() %>% 
    legend() %>% 
    points(1:4, 1:4, legend.name = "first points")
  
  expect_equal(gs$legend$legend.auto$legend, "first points")
})
