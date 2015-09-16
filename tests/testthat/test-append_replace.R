context("test append replace list")
test_that("append replace list", {
  
  old.list <- list(one=3, two=5, four = 4)
  new.list <- list(one=1, two=2, five=5)
  list = gsplot:::append_replace(old.list, new.list)
  
  expect_equal(list, list(one=1,two=2, four=4, five=5))
  
})

test_that("append replace list out of order", {
  
  old.list <- list(one=3, two=5, four = 4)
  new.list <- list(two=2, one=1, five=5)
  list = gsplot:::append_replace(old.list, new.list)
  
  expect_equal(list, list(one=1,two=2, four=4, five=5))
  
})
