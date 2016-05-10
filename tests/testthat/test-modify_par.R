context("modify global par")
test_that("global par arguments", {
  
  arguments = list(x=3, y=34, col='yellow',las=3)
  gs <- gsplot()
  expect_error(gsplot:::modify_global_par(gs, arguments),'cannot set x, y')
  expect_equal(gsplot:::modify_global_par(gs, list(col='yellow'))$global$par$col, 'yellow')
})

context("modify view par")
test_that("view par arguments skip las", {
  
  arguments = list(col='yellow',las=3)
  gs <- gsplot()
  expect_false('las' %in% names(gsplot:::modify_view_par(gs, arguments, side=1)$view.1.2$par))
  
})
test_that("view par arguments skip ylim", {
  
  arguments = list(col='yellow',ylim=c(1,10))
  gs <- gsplot()
  expect_false('ylim' %in% names(gsplot:::modify_view_par(gs, arguments, side=1)$view.1.2$par))
  
})

test_that("view par arguments doesn't skip col", {
  
  arguments = list(col='yellow',las=3)
  gs <- gsplot()
  expect_true('col' %in% names(gsplot:::modify_view_par(gs, arguments, side=1)$view.1.2$par))
})


context("modify side par")
test_that("side par arguments includes las", {
  
  arguments = list(col='yellow',las=3)
  gs <- gsplot()
  expect_true('las' %in% names(gsplot:::modify_side_par(gs, arguments, side=1)$side.1$par))
  
})

test_that("side par arguments includes ylim", {
  
  arguments = list(col='yellow',ylim=c(1,10))
  gs <- gsplot()
  expect_false('ylim' %in% names(gsplot:::modify_side_par(gs, arguments, side=1)$side.1$par))
  
})

test_that("side par warning/stop works", {
  
  arguments = list(col='yellow')
  gs <- gsplot()
  expect_warning(gsplot:::modify_side_par(gs, arguments, side=1, on.readonly='warning'), 
                 "cannot set col; arguments will be ignored")
  expect_error(gsplot:::modify_side_par(gs, arguments, side=1, on.readonly='stop'), 
                 "cannot set col")
})