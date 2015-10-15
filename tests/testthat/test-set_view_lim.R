context("set_view_lim")

test_that("gsplot xaxs = 'o' works", {
  gs <- gsplot() %>%
    points(1:4,1:4, xlim=c(NA,10), xaxs='o') %>%
    lines(1:6,1:6)
  
  beforeBuffer <- c(1,10)
  buffer <- diff(beforeBuffer)*0.04
  afterBuffer <- c(beforeBuffer[1]-buffer, beforeBuffer[2])
  
  expect_equal(xlim(gs), afterBuffer)
  expect_equal(par(gs)$xaxs, "i")
})


test_that("gsplot yaxs = 'o' works", {
  gs <- gsplot() %>%
    points(1:4,1:4, ylim=c(0,NA), yaxs='o') %>%
    lines(1:6,1:6)
  
  beforeBuffer <- c(0,4)
  buffer <- diff(beforeBuffer)*0.04
  afterBuffer <- c(beforeBuffer[1], beforeBuffer[2]+buffer)
  
  expect_equal(ylim(gs), afterBuffer)
  expect_equal(par(gs)$yaxs, "i")
})

test_that("gsplot yaxs = 'o' works", {
  gs <- gsplot(ylim=c(0,NA), yaxs='o') %>%
    points(1:4,1:4) %>%
    lines(1:6,1:6)
  
  beforeBuffer <- c(0,6)
  buffer <- diff(beforeBuffer)*0.04
  afterBuffer <- c(beforeBuffer[1], beforeBuffer[2]+buffer)
  
  expect_equal(ylim(gs), afterBuffer)
  expect_equal(par(gs)$yaxs, "i")
})