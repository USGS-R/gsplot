context("test date utilities")
test_that("day period works for Date type", {
  data_period <- c(as.Date("2001-04-05"), as.Date("2001-04-17"))
  axis_period <- gsplot:::day_period(data_period)
  expect_equal(axis_period, data_period)
})

test_that("day period works for POSIXct type", {
  data_period <- c(as.POSIXct("2001-04-05 05:00:00"), as.POSIXct("2001-04-17 17:00:00"))
  axis_period <- gsplot:::day_period(data_period)
  expect_equal(axis_period, c(as.POSIXct("2001-04-05 00:00:00"), as.POSIXct("2001-04-17 23:59:59")))
})

test_that("week period works for Date type", {
  data_period <- c(as.Date("2017-02-02"), as.Date("2017-02-09"))
  axis_period <- gsplot:::week_period(data_period)
  expect_equal(axis_period, c(as.Date("2017-01-29"), as.Date("2017-02-11")))
})

test_that("week period works for POSIXct type", {
  data_period <- c(as.POSIXct("2017-02-04 05:00:00"), as.POSIXct("2017-02-09 17:00:00"))
  axis_period <- gsplot:::week_period(data_period)
  expect_equal(axis_period, c(as.POSIXct("2017-01-29 00:00:00"), as.POSIXct("2017-02-11 23:59:59")))
})

test_that("month period works for Date type", {
  data_period <- c(as.Date("1983-04-04"), as.Date("1983-08-22"))
  axis_period <- gsplot:::month_period(data_period)
  expect_equal(axis_period, c(as.Date("1983-04-01"), as.Date("1983-08-31")))
})

test_that("month period works for POSIXct type", {
  data_period <- c(as.POSIXct("1983-04-04 05:00:00"), as.POSIXct("1983-08-22 17:00:00"))
  axis_period <- gsplot:::month_period(data_period)
  expect_equal(axis_period, c(as.POSIXct("1983-04-01 00:00:00"), as.POSIXct("1983-08-31 23:59:59")))
})

test_that("month period handles leap year ok", {
  data_period <- c(as.Date("2000-02-02"), as.Date("2000-02-20"))
  axis_period <- gsplot:::month_period(data_period)
  expect_equal(axis_period, c(as.Date("2000-02-01"), as.Date("2000-02-29")))
})

test_that("quarter period works for Date type", {
  data_period <- c(as.Date("2007-11-07"), as.Date("2007-12-15"))
  axis_period <- gsplot:::quarter_period(data_period)
  expect_equal(axis_period, c(as.Date("2007-10-01"), as.Date("2007-12-31")))
})

test_that("quarter period works for POSIXct type", {
  data_period <- c(as.POSIXct("2007-11-07 05:00:00"), as.POSIXct("2007-12-15 17:00:00"))
  axis_period <- gsplot:::quarter_period(data_period)
  expect_equal(axis_period, c(as.POSIXct("2007-10-01 00:00:00"), as.POSIXct("2007-12-31 23:59:59")))
})

test_that("year period works for Date type", {
  data_period <- c(as.Date("2022-05-14"), as.Date("2025-07-03"))
  axis_period <- gsplot:::year_period(data_period)
  expect_equal(axis_period, c(as.Date("2022-01-01"), as.Date("2025-12-31")))
})

test_that("year period works for POSIXct type", {
  data_period <- c(as.POSIXct("2022-05-14 05:00:00"), as.POSIXct("2025-07-03 17:00:00"))
  axis_period <- gsplot:::year_period(data_period)
  expect_equal(axis_period, c(as.POSIXct("2022-01-01 00:00:00"), as.POSIXct("2025-12-31 23:59:59")))
})

test_that("water year period works for Date type", {
  data_period <- c(as.Date("2011-07-14"), as.Date("2011-11-02"))
  axis_period <- gsplot:::wateryear_period(data_period)
  expect_equal(axis_period, c(as.Date("2010-10-01"), as.Date("2012-09-30")))
})

test_that("water year period works for POSIXct type", {
  data_period <- c(as.POSIXct("2011-07-14 05:00:00"), as.POSIXct("2011-11-02 17:00:00"))
  axis_period <- gsplot:::wateryear_period(data_period)
  expect_equal(axis_period, c(as.POSIXct("2010-10-01 00:00:00"), as.POSIXct("2012-09-30 23:59:59")))
})

test_that("decade period works for Date type", {
  data_period <- c(as.Date("1992-03-04"), as.Date("2001-05-03"))
  axis_period <- gsplot:::decade_period(data_period)
  expect_equal(axis_period, c(as.Date("1990-01-01"), as.Date("2009-12-31")))
})

test_that("decade period works for POSIXct type", {
  data_period <- c(as.POSIXct("1992-03-04 05:00:00"), as.POSIXct("2001-05-03 17:00:00"))
  axis_period <- gsplot:::decade_period(data_period)
  expect_equal(axis_period, c(as.POSIXct("1990-01-01 00:00:00"), as.POSIXct("2009-12-31 23:59:59")))
})
