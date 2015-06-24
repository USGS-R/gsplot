context("basic")

test_that("Basic tests", {
  testthat::skip_on_cran()

  expect_that(1 == 1, is_true())

})
