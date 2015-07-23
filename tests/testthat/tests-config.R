context("config")

test_that("lists and named args are identical", {
  expect_equal(gsplot:::config("par",list("mar"=c(1,2,3,4)))$mar, c(1,2,3,4))
  expect_equal(gsplot:::config("par",mar=c(1,2,3,4))$mar, c(1,2,3,4))
})

test_that("empty sets on gsplot are ignored",{
  expect_is(gsplot:::config("par"), 'list')
})