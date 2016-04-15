context("test simple sides utilities")
test_that("naming sides", {
  expect_equal(gsplot::as.side_name(1), 'side.1')
  expect_equal(gsplot::as.side_name(c(1,2)), c('side.1','side.2'))
  expect_equal(gsplot::as.side_name('side.1'), 'side.1')
  expect_equal(gsplot::as.side_name('view.1.2'), c('side.1','side.2'))
})