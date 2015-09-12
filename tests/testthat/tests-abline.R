context("abline")

test_that("abline",{
  
  ## Setup up coordinate system (with x == y aspect ratio):
  plot(c(-2,3), c(-1,5), type = "n", xlab = "x", ylab = "y", asp = 1)
  ## the x- and y-axis, and an integer grid
  abline(h = 0, v = 0, col = "gray60")
  
})

test_that("abline gsplot",{
  gs = points(gsplot(), c(-2,3), c(-1,5)) %>% 
    abline(h = 0, v = 0, col = "gray60")
  expect_equal(names(gs$view), c("points","abline","window"))
})

test_that("segments",{
  x <- stats::runif(12); y <- stats::rnorm(12)
  i <- order(x, y); x <- x[i]; y <- y[i]
  plot(x, y, main = "arrows(.) and segments(.)")
  ## draw arrows from point to point :
  s <- seq(length(x)-1)  # one shorter than data
  arrows(x[s], y[s], x[s+1], y[s+1], col= 1:3)
  s <- s[-length(s)]
  segments(x[s], y[s], x[s+2], y[s+2], col= 'pink')
})

test_that("segments gsplot",{
  x <- stats::runif(12); y <- stats::rnorm(12)
  i <- order(x, y); x <- x[i]; y <- y[i]
  gs = points(gsplot(), x, y, main = "arrows(.) and segments(.)")
  ## draw arrows from point to point :
  s <- seq(length(x)-1)  # one shorter than data
  gs = arrows(gs, x[s], y[s], x[s+1], y[s+1], col= 1:3)
  s <- s[-length(s)]
  gs = segments(gs, x[s], y[s], x[s+2], y[s+2], col= 'pink')
  expect_equal(names(gs$view), c("points","arrows","segments","window"))
  
})