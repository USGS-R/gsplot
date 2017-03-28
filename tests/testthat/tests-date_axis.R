context("date_axis adds the correct axis calls")

test_that("two axis calls added to side 1", {
  gs <- gsplot() %>%
    points(seq(as.Date("2013-01-01"), as.Date("2013-01-31"), "days"), 1:31) %>%
    date_axis(side=1, lab.pos = "interval", tick.int="day", "snap.to"="month")
  
  expect_equal(length(grep(pattern = "axis", names(gs[['side.1']]))), 2)
})

test_that("axis ticks in the right location", {
  gs <- gsplot() %>%
    points(seq(as.Date("2013-01-01"), as.Date("2013-12-31"), "days"), 1:365) %>%
    date_axis(side=1, lab.pos = "interval", tick.int="month", "snap.to"="month")
  
  ticks <- lazy_eval(gs$side.1$axis$at, data=list(object=gs))
  expect_equal(ticks[1], as.Date("2013-01-01"))
  expect_equal(ticks[7], as.Date("2013-07-01"))
  expect_equal(ticks[12], as.Date("2013-12-01"))
})

test_that("axis labels centered on interval", {
  gs <- gsplot() %>%
    points(seq(as.Date("2013-01-01"), as.Date("2013-12-31"), "days"), 1:365) %>%
    date_axis(side=1, lab.pos = "interval", tick.int="month", "snap.to"="month")
  
  second.axis <- gs$side.1
  which.axis <- which(names(second.axis)== 'axis')
  labels <- lazyeval::lazy_eval(gs$side.1[[which.axis[2]]]$at, data=list(object=gs))
  expect_true(all.equal(labels[1], as.Date("2013-01-15"), tolerance=0.01))
  expect_true(all.equal(labels[7], as.Date("2013-07-15"), tolerance=0.01))
  expect_true(all.equal(labels[12], as.Date("2013-12-15"), tolerance=0.01))
})

test_that("axis labels centered on ticks", {
  gs <- gsplot() %>%
    points(seq(as.Date("2013-01-01"), as.Date("2013-01-31"), "days"), 1:31) %>%
    date_axis(side=1, lab.pos = "tick", tick.int="day", "snap.to"="month")
  
  second.axis <- gs$side.1
  which.axis <- which(names(second.axis)== 'axis')
  
  labels <- lazy_eval(gs$side.1[[which.axis[2]]]$at, data=list(object=gs))
  expect_equal(labels[1], as.Date("2013-01-01"))
  expect_equal(labels[7], as.Date("2013-01-07"))
  expect_equal(labels[22], as.Date("2013-01-22"))
})