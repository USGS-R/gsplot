context("points")

test_that("graphics examples work", {
  
  plot(-4:4, -4:4, type = "n")  # setting up coord. system
  points(rnorm(200), rnorm(200), col = "red")
  
  dev.off()
  plot(-4:4, -4:4, type = "n", xlim=c(0,100000), ylim=c(0,1))  # setting up coord. system
  lx <- seq(1, 5, length = 41)
  xy = xy.coords(x=10^lx,y=exp(-.5*lx^2))
  plot.xy(xy, type='p')
  
  plot(1,3)
  points(data.frame(1,2), col='red')
  
})

context("points arguments")
test_that("setting params works as expected",{
  #not specifying y in points call works (passed through function_args, fixed in set_args)
  expect_equal(gsplot:::function_args("graphics","points", 5, y = NULL), list(x=5, y=NULL))
  expect_equal(gsplot:::set_args("points", 5, y = NULL, package="graphics"), 
               list(x=1, y=5, pch=6, col="red"))
  
  #function_args gives back values of x and y unchanged
  expect_equal(gsplot:::function_args("graphics","points", y=5, x=0), list(x=0, y=5))
})

test_that("setting non-formal params works as expected",{
  expect_equal(gsplot:::function_args("graphics","points", y=5, x=0, col='blue'), list(x=0, y=5, col='blue'))
  expect_equal(gsplot:::function_args("graphics","points", y=5, x=0, lty=2, col='red'), list(x=0, y=5, lty=2, col='red'))
})

test_that("testing content of gsplot list", {
  
  gs <- gsplot()
  
  expect_is(gs,"gsplot")
  
  gs <- points(gs, y=1, x=2, col="blue", pch=18)
  
  expect_equal(gs$view$points$x, 2)
  
  expect_false(gs$view$points$col=="green")
  
  expect_equal(gs$view$points$pch,18)
    
  
})

test_that("override works w/ formulas",{
  dev.off()
  plot(-4:4, -4:4, type = "n") 
  points(y~x, data=list(x=-3:3,y=-3:3))  # // no errors
})

test_that("points.gsplot accepts formulas",{
  
  warning('skipping points formula test')
  # gs <- gsplot() %>%
  #      points(y~x, data=list(x=-3:3,y=-3:3))
  # expect_equal(xlim(gs)[[1]], c(-3,3))
  # expect_equal(ylim(gs)[[1]], c(-3,3))
  # gs
           
})

test_that("points works with indexing", {
  
  x <- 7:10
  gs <- gsplot() %>%
    points(x)
  expect_equal(class(gs$view.1.2$points$y), "integer")
  expect_equal(gs$view.1.2$points$x, seq_along(x))
  

  oct_dates <- seq(as.Date("2015-10-11"), as.Date("2015-10-15"), by="days")
  gs_dates <- gsplot() %>%
    points(oct_dates)
  expect_equal(class(gs_dates$view.1.2$points$y), "Date")
  expect_equal(gs_dates$view.1.2$points$x, seq_along(oct_dates))
})
