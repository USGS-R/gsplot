context("set-side-lim")

test_that("class retained: NA to data", {
  
  gs <- gsplot() %>% lines(NA, NA)
  expect_is(gs$side.1$lim, "logical")
  expect_is(gs$side.2$lim, "logical")
  
  gs.data <- gs %>% points(as.Date("2010-10-01"), y=5)
  
  expect_is(gs.data$side.1$lim, "Date")
  expect_is(gs.data$side.2$lim, "numeric")
  
})

test_that("class retained: data to NA", {
  
  gs.data <- gsplot() %>% points(as.Date("2010-10-01"), y=5)
  expect_is(gs.data$side.1$lim, "Date")
  expect_is(gs.data$side.2$lim, "numeric")
  
  gs <- gs.data %>% lines(NA,NA)
  expect_is(gs$side.1$lim, "Date")
  expect_is(gs$side.2$lim, "numeric")
  
})

test_that("class retained: no data", {

  date_lims <- c(as.POSIXct("2010-01-01"), as.POSIXct("2015-01-01"))
  gs <- gsplot() %>% lines(as.POSIXct(NA), as.numeric(NA), xlim=date_lims, ylim=c(0,100))
  # side 1 limits came back as numeric before fix to retain classes
  expect_is(gs$side.1$lim, "POSIXct")

})

