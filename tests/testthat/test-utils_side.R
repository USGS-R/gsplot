context("test simple sides utilities")
test_that("naming sides", {
  expect_equal(as.side_name(1), 'side.1')
  expect_equal(as.side_name(c(1,2)), c('side.1','side.2'))
  expect_equal(as.side_name('side.1'), 'side.1')
  expect_equal(as.side_name('view.1.2'), c('side.1','side.2'))
})

test_that("side getter returns expected info", {
  gs <- gsplot() %>%
    points(1:10, 1:10)
  expect_named(gsplot:::sides(gs, 1), "side.1")
  expect_equal(length(gsplot:::sides(gs, c(1,2))), 2)
  expect_equal(length(gsplot:::sides(gs, 3)), 0)
})