context("test set lim for gsplot object")
test_that("set lim for single axis", {
  
  usrDef <- gsplot() %>% 
    points(x=1, y=2, side=c(3,2), legend.name="Points 1", cex=3, xlab='cat')
  
  
  expect_equal(lim(usrDef, 3), c(1,1))
  lim(usrDef, side=3) <- c(0.2, 1.2)
  
  expect_equal(lim(usrDef, 3), c(0.2, 1.2))
})

test_that("set lim usr.lim", {
  
  usrDef <- gsplot() %>% 
    points(x=1, y=2, side=c(3,2), legend.name="Points 1", cex=3, xlab='cat')
  
  
  expect_false(usrDef$side.3$usr.lim[1])
  expect_false(usrDef$side.3$usr.lim[2])
  lim(usrDef, side=3)[1] <- 0.2
  expect_true(usrDef$side.3$usr.lim[1])
  expect_false(usrDef$side.3$usr.lim[2])
  
  warning("should test that you can't set a limit for a side that doesn't exist")
})

test_that("set lim w/ NA", {
  
  usrDef <- gsplot() %>% 
    points(x=1, y=2, side=c(3,2), legend.name="Points 1", cex=3, xlab='cat')
  
  lim(usrDef, side=3) <- c(0.2,NA)
  expect_true(usrDef$side.3$usr.lim[1])
  expect_false(usrDef$side.3$usr.lim[2])
  usrDef <- points(usrDef, 5,2, side=c(3,2))
  expect_equal(lim(usrDef, 3)[2], 5)
})