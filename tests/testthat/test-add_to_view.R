context("test add new view")
test_that("add new view", {
  
  obj <- list()
  obj <- gsplot:::add_new_view(obj, 'view.1.2')
  expect_true('view.1.2' %in% gsplot:::view_names(obj))
  
  expect_error(gsplot:::add_new_view(obj, 'view.1.2'), 'view.1.2 already exists, cannot add it.')
  
  
})

test_that("adding a new view in proper place",{
  obj <- list()
  obj <- gsplot:::add_new_view(obj, 'view.1.2')
  obj <- append(obj, list('not.view'=c()))
  obj <- gsplot:::add_new_view(obj, 'view.1.4')
  expect_equal(gsplot:::which_views(obj), c(1,2))
})

context("test add function to view")
test_that("add new function", {
  obj <- gsplot:::add_new_view(list(), 'view.1.2')
  call.args <- list('points'=list('x'=1:3, 'y'=3:5))
  obj <- gsplot:::add_to_view(obj, call.args, side=1)
  expect_equal(obj[['view.1.2']][['points']][['x']], c(1,2,3))
  obj <- gsplot:::add_to_view(obj, call.args, side=1)
  call.args <- list('lines'=list('x'=1:3, 'y'=3:5))
  obj <- gsplot:::add_to_view(obj, call.args, side=1)
  expect_equal(names(obj[['view.1.2']]), c('par','points','points','lines'))
})

context("test call arguments")

test_that("call args don't have side arguments",{
  call.args <- gsplot:::call_arguments('points', x=2:6, y=2:6, ylim=c(-1, 11))
  expect_null(call.args$points$ylim)
  expect_equal(call.args$points$y, c(2:6))
  
})

test_that("embedded arguments are captured",{
  call.args <- gsplot:::call_arguments('points', x=1:5, y=1:5, xlim=c(0,10), ylim=c(0,10), 
                                       callouts(labels=c(rep(NA, 4), "oh")))
  expect_null(call.args$points$ylim)
  expect_equal(call.args$callouts$y, c(1:5))
  
})

test_that("can embed a graphics function",{
  call.args <- gsplot:::call_arguments('points', x=1:5, y=1:5, xlim=c(0,10), ylim=c(0,10), lines(col='red'))
  expect_equal(call.args$lines$y, c(1:5))
})