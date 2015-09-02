context("segments arguments")
test_that("setting params works as expected",{
  expect_equal(gsplot:::function_args("graphics","segments",x0=1, y0=2, 3, 4), list(x0=1, y0=2, x1=3, y1=4))
  expect_equal(gsplot:::function_args("graphics","segments",1, y0=2, 3, 4), list(x0=1, y0=2, x1=3, y1=4))
  expect_equal(gsplot:::function_args("graphics","segments",1, 2, y1=4, 3), list(x0=1, y0=2, x1=3, y1=4))
})