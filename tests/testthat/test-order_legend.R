context("test 'where' argument for legend")

test_that("default legend order", {
  gs = gsplot() %>% points(1,2, cex=3.5, pch=0, lwd=3, where='last', legend.name='points.1') %>% 
    points(1,2, pch=17, col='green', cex=4, where='last', legend.name='points.2')
  
  gs.def = gsplot() %>% points(1,2, cex=3.5, pch=0, lwd=3, legend.name='points.1') %>% 
    points(1,2, pch=17, col='green', cex=4, legend.name='points.2')
  expect_equal(gs$legend, gs.def$legend)
})

test_that("simple legend vectors, re-ordered", {
  
  gs = gsplot() %>% points(1,2, cex=3.5, pch=0, lwd=3, legend.name='points.1', bg='cyan') %>% 
    points(1,2, pch=17, col='green', cex=4, where='first', legend.name='points.2', bg='dodgerblue')
  
  expect_equal(gs$legend$legend.auto$legend, c("points.2", "points.1"))
  expect_equal(gs$legend$legend.auto$col, c("green", "red"))
  expect_equal(gs$legend$legend.auto$pt.bg, c("dodgerblue", "cyan"))
  
})

test_that("simple legend vectors, default order", {
  
  gs = gsplot() %>% points(1,2, cex=3.5, pch=0, lwd=3, legend.name='points.1', bg='cyan') %>% 
    points(1,2, pch=17, col='green', cex=4, legend.name='points.2', bg='dodgerblue')
  
  expect_equal(gs$legend$legend.auto$legend, c("points.1", "points.2"))
  expect_equal(gs$legend$legend.auto$col, c("red", "green"))
  expect_equal(gs$legend$legend.auto$pt.bg, c("cyan", "dodgerblue"))
  
})

test_that("multi-named legend calls", {
  
  gs = gsplot() %>% points(1:2,2:3, cex=3.5, pch=c(0,3), lwd=3, legend.name=c('points.1','points.2'), bg='cyan') %>% 
    points(1,2, pch=17, col='green', cex=4, where='first', legend.name='points.3', bg='dodgerblue')
  
  expect_equal(gs$legend$legend.auto$legend, c("points.3", "points.1", "points.2"))
  expect_equal(gs$legend$legend.auto$col, c("green", "red", "red"))
  expect_equal(gs$legend$legend.auto$pt.bg, c("dodgerblue", "cyan", "cyan"))
  expect_equal(gs$legend$legend.auto$pch, c(17, 0, 3))
  
})