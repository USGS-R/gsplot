context("axis")

test_that("axis",{
  
  ## Setup up coordinate system (with x == y aspect ratio):
  plot(c(-2,3), c(-1,5), axes=FALSE)
  ## the x- and y-axis, and an integer grid
  axis(1)
  axis(2)
  axis(3)
  
})

test_that("axis gsplot",{
  gs = points(gsplot(mar=c(1,1,1,1)), c(-2,3), c(-1,5)) %>% 
    axis(3)
  expect_true(all(names(gs) %in% c("side.1", "side.2", "side.3", "view.1.2", "global")))
  
  gs <- gsplot() %>%
     lines(1:5, c(1,10,100,1000,10000), log="y", axes=FALSE) %>%
     axis(side=c(2,4), labels=FALSE, n.minor=4)
  
  expect_false(gs$side.1$axes)
  expect_false(gs$side.2$axes)
  
})

test_that("axis reverse",{
  
  gs <- gsplot() %>%
    points(1:10, 1:10) %>%
    axis(1, at = seq(0,10,by=0.1),labels=FALSE, tcl=0.15) %>%
    axis(2, reverse=TRUE)
  
  expect_true(gs$side.2$reverse)
  expect_equal(ylim(gs)$side.2, c(10,1))
  
  gs2 <- gsplot() %>%
    points(1:10, 1:10) %>%
    axis(1, at = seq(0,10,by=0.1),labels=FALSE, tcl=0.15) %>%
    axis(3, reverse=TRUE)
  
  expect_true(gs2$side.3$reverse)
  expect_equal(ylim(gs2)$side.3, c(10,1))
  
})

