context("set_view_lim")

test_that("gsplot xaxs = 'o' works", {
  gs <- gsplot(xlim=c(NA,10), xaxs='o') %>%
    points(1:4,1:4) %>%
    lines(1:6,1:6)
  gs
  
  beforeBuffer <- c(1,10)
  buffer <- diff(beforeBuffer)*0.04
  afterBuffer <- c(beforeBuffer[1]-buffer, beforeBuffer[2])
  
#   expect_equal(xlim(gs), afterBuffer)
#   expect_equal(par(gs)$xaxs, "i")
})

test_that("gsplot yaxs = 'o')", {
  gs <- gsplot(ylim=c(0,NA), yaxs='o') %>%
    points(1:4,1:4) %>%
    lines(1:6,1:6)
  
  beforeBuffer <- c(0,6)
  buffer <- diff(beforeBuffer)*0.04
  afterBuffer <- c(beforeBuffer[1], beforeBuffer[2]+buffer)
  
#   expect_equal(ylim(gs), afterBuffer)
#   expect_equal(par(gs)$yaxs, "i")
})