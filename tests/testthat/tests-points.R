context("points")

test_that("graphics examples work", {
  
  plot(-4:4, -4:4, type = "n")  # setting up coord. system
  points(rnorm(200), rnorm(200), col = "red")

})

