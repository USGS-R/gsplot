context("test 'where' argument for plotting")
test_that("where fails when not last or first", {
  
  expect_error(gsplot() %>% points(1,2, where='fiasdf'))
  expect_is(gsplot() %>% points(1,2, where='first'), 'gsplot')
  expect_is(gsplot() %>% points(1,2, where='last'), 'gsplot')
  
})

test_that("where param is honored", {
  gs <- gsplot() %>% lines(1,2) %>% points(1,2, where='first')
  p.i <- which(names(gs$view.1.2) == 'points')
  l.i <- which(names(gs$view.1.2) == 'lines')
  expect_gt(l.i, p.i)
  gs <- gsplot() %>% lines(1,2) %>% points(1,2, where='last')
  p.i <- which(names(gs$view.1.2) == 'points')
  l.i <- which(names(gs$view.1.2) == 'lines')
  expect_lt(l.i, p.i)
})
