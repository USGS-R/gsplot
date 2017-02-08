context("grid lines")

test_that('automatic grid lines work', {
  gs <- gsplot() %>% 
    points(1:10,1:10) %>% 
    grid()
  
  expect_identical(gs$view.1.2$grid$col, "grey")
  expect_equal(gs$view.1.2$grid$lwd, 1)
  expect_equal(gs$view.1.2$grid$lty, 2)
  expect_null(gs$view.1.2$grid$nx)
  expect_null(gs$view.1.2$grid$ny)
})

test_that('grid lines work for horizontal and vertical', {
  gs <- gsplot() %>% 
    points(1:10,1:10) %>% 
    grid(nx=10, ny=5)
  
  expect_identical(gs$view.1.2$grid$col, "grey")
  expect_equal(gs$view.1.2$grid$lwd, 1)
  expect_equal(gs$view.1.2$grid$lty, 2)
  expect_equal(gs$view.1.2$grid$nx, 10)
  expect_equal(gs$view.1.2$grid$ny, 5)
})

test_that('grid lines work vertically', {
  gs <- gsplot() %>% 
    points(1:10,1:10) %>% 
    grid(nx=10, ny=0)
  
  expect_identical(gs$view.1.2$grid$col, "grey")
  expect_equal(gs$view.1.2$grid$lwd, 1)
  expect_equal(gs$view.1.2$grid$lty, 2)
  expect_equal(gs$view.1.2$grid$nx, 10)
  expect_equal(gs$view.1.2$grid$ny, 0)
  
  gs.NA <- gsplot() %>% 
    points(1:10,1:10) %>% 
    grid(nx=10, ny=NA)
  
  expect_true(is.na(gs.NA$view.1.2$grid$ny))
})

test_that('grid lines work horizontally', {
  gs <- gsplot() %>% 
    points(1:10,1:10) %>% 
    grid(nx=0, ny=10)
  
  expect_identical(gs$view.1.2$grid$col, "grey")
  expect_equal(gs$view.1.2$grid$lwd, 1)
  expect_equal(gs$view.1.2$grid$lty, 2)
  expect_equal(gs$view.1.2$grid$ny, 10)
  expect_equal(gs$view.1.2$grid$nx, 0)
  
  gs.NA <- gsplot() %>% 
    points(1:10,1:10) %>% 
    grid(nx=NA, ny=10)
  
  expect_true(is.na(gs.NA$view.1.2$grid$nx))
})

test_that('grid args are passed through', {
  gs <- gsplot() %>% 
    points(1:10,1:10) %>% 
    grid(col="lightblue", lty="dashed")
  
  expect_identical(gs$view.1.2$grid$col, "lightblue")
  expect_identical(gs$view.1.2$grid$lty, "dashed")
})

test_that('grid ny/nx args should not cause a warning', {
  gs <- gsplot() %>% 
    points(1:10,1:10) %>% 
    grid(nx=2, ny=10)
  
  expect_silent(gs)
})

test_that('grid works with POSIX', {
  x.posix <- seq(as.POSIXct("2010-10-01"), as.POSIXct("2010-10-31"), by="days")
  gs <- gsplot() %>% 
    points(x.posix,1:31) %>% 
    grid(ny=NA)
  
  expect_silent(gs)
})

test_that('grid works with dates', {
  x.dates <- seq(as.Date("2010-10-01"), as.Date("2010-10-31"), by="days")
  gs <- gsplot() %>% 
    points(x.dates,1:31) %>% 
    grid(ny=NA)
  
  expect_silent(gs)
})

test_that("testing grid config", {
  
  gsp <- gsplot(config.file = system.file("extdata", "lineScatter.yaml", package = "gsplot")) %>%
    grid()
  
  expect_true(gsp$global$config$config.file)
  expect_equal(gsp$view.1.2$grid$col, "gray80")
  
  gsp <- gsplot() %>%
    grid()
  
  expect_false(gsp$global$config$config.file)
  expect_equal(gsp$view.1.2$grid$col, "grey") #This is what our default currently is
  
})