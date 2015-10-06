context("test get xlim for gsplot object")
test_that("xlim for single axis", {
  
  usrDef <- gsplot(mar=c(4,4,4,4), xaxs='r', yaxs='r') %>% 
    points(x=1, y=2, side=c(3,2), legend.name="Points 1", cex=3, xlab='cat')
  
  expect_equal(xlim(usrDef)[[1]], c(1,1))
  
  usrDef <- points(usrDef,x=3:10,y=4:11, side=c(3,2))
  
  expect_equal(xlim(usrDef)[[1]], c(1,10))
  
})

test_that("xlim for dual axis", {
  
  usrDef <- gsplot(mar=c(4,4,4,4), xaxs='r', yaxs='r') %>% 
    points(x=1, y=2, side=c(3,2), legend.name="Points 1", cex=3, xlab='cat') %>% 
    points(x=3:10,y=4:11, side=c(1,2))
  
  expect_equal(xlim(usrDef)[[1]], c(1,1))
  expect_equal(xlim(usrDef)[[2]], c(3,10))
  
})

test_that("xlim for string input", {
  
  usrDef <- gsplot(mar=c(4,4,4,4), xaxs='r', yaxs='r') %>% 
    points(x=1, y=2, side=c(3,2), legend.name="Points 1", cex=3, xlab='cat') %>% 
    points(x=3:10,y=4:11, side=c(1,2))
  
  expect_equal(xlim(usrDef)[['side.3']], c(1,1))
  expect_equal(xlim(usrDef)[['side.1']], c(3,10))
  
})

context("test get par for gsplot object")

test_that("par for simple object",{
  expect_equal(par(gsplot(mar=c(4,4,4,4), xaxs='r', yaxs='r'))$xaxs,'r')
  expect_is(par(gsplot(mar=c(4,4,4,4), xaxs='r', yaxs='r')), 'list')
})

context("test get log for gsplot object")

test_that("log for simple object",{
  usrDef <- gsplot(mar=c(4,4,4,4), xaxs='r', yaxs='r') %>% 
    points(x=1, y=2, side=c(3,2), legend.name="Points 1", cex=3, xlab='cat') %>% 
    points(x=3:10,y=4:11, side=c(1,2))
  expect_equal(log(usrDef)[[1]],"")
  expect_equal(log(usrDef)[[2]],"")
  
  usrDef <- lines(usrDef,1:2,4:5, log='xy')
  expect_equal(log(usrDef)[[1]],"")
  expect_equal(log(usrDef)[[2]],"xy")
  
})

test_that("base::log still works as expected",{
  expect_equal(log(10),base::log(10))
})