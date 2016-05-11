context("test add new side")
test_that("add new side", {
  
  obj <- list()
  obj <- gsplot:::add_new_side(obj, 'side.1')
  expect_true('side.1' %in% gsplot:::side_names(obj))
  
  expect_error(gsplot:::add_new_side(obj, 'side.1'), 'side.1 already exists, cannot add it.')
})

test_that("par defaults on new side", {
  
  obj <- list()
  obj <- gsplot:::add_new_side(obj, 'side.1')
  obj <- gsplot:::modify_side_par(obj, arguments =list(las='2'), side=1)
  expect_true('par' %in% names(obj[['side.1']]))
})

test_that("axis defaults on new side", {
  
  obj <- list()
  obj <- gsplot:::add_new_side(obj, 'side.1')
  expect_true(all(c("lim", "log", "label", "usr.lim") %in% names(obj[['side.1']])))
  expect_false(any(obj[['side.1']][['usr.lim']]))
})