context("grid lines")

test_that('automatic grid lines work', {
  gs <- gsplot() %>% 
    points(1:10,1:10) %>% 
    grid()
  
  expect_identical(gs$view.1.2$grid$col, "grey")
  expect_identical(gs$view.1.2$grid$lwd, 1)
  expect_identical(gs$view.1.2$grid$lty, 2)
  expect_null(gs$view.1.2$grid$nx)
  expect_null(gs$view.1.2$grid$ny)
})

test_that('grid lines work for horizontal and vertical', {
  gs <- gsplot() %>% 
    points(1:10,1:10) %>% 
    grid(nx=10, ny=5)
  
  expect_identical(gs$view.1.2$grid$col, "grey")
  expect_identical(gs$view.1.2$grid$lwd, 1)
  expect_identical(gs$view.1.2$grid$lty, 2)
  expect_identical(gs$view.1.2$grid$nx, 10)
  expect_identical(gs$view.1.2$grid$ny, 5)
})

test_that('grid lines work vertically', {
  gs <- gsplot() %>% 
    points(1:10,1:10) %>% 
    grid(nx=10, ny=0)
  
  expect_identical(gs$view.1.2$grid$col, "grey")
  expect_identical(gs$view.1.2$grid$lwd, 1)
  expect_identical(gs$view.1.2$grid$lty, 2)
  expect_identical(gs$view.1.2$grid$nx, 10)
  expect_identical(gs$view.1.2$grid$ny, 0)
  
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
  expect_identical(gs$view.1.2$grid$lwd, 1)
  expect_identical(gs$view.1.2$grid$lty, 2)
  expect_identical(gs$view.1.2$grid$ny, 10)
  expect_identical(gs$view.1.2$grid$nx, 0)
  
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
