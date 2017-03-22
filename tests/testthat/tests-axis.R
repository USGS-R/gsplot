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
  expect_true(all(names(gs) %in% c("side.1", "side.2", "side.3", 
                                   "view.1.2", "global", "metadata")))
  
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
  expect_equal(ylim(gs, side=2), c(10,1))
  
  gs2 <- gsplot() %>%
    points(1:10, 1:10, side=c(3,2)) %>%
    points(1:10, 1:10, side=c(1,2)) %>% 
    axis(3, reverse=TRUE)
  
  expect_true(gs2$side.3$reverse)
  expect_equal(xlim(gs2, side=3), c(10,1))
  expect_warning(ylim(gs2, side=3))
  
})

context('multiple axis on the same side can be used')
test_that("axis can append a second one",{
  gs <- gsplot() %>% 
    points(0:1,0:1) %>% 
    axis(side=1, at=c(0.5,1)) %>% 
    axis(side=1, at=c(0.25, 0.75), append=TRUE)
  # expect_equal(sum(names(gs$side.1) == 'axis'), 2)
})

test_that("axis can append a third one and the forth clears them",{
  gs <- gsplot() %>% 
    points(0:1,0:1) %>% 
    axis(side=1, at=c(0.5,1)) %>% 
    axis(side=1, at=c(0.25, 0.75), append=TRUE) %>% 
    axis(side=1, at=c(0.45, 0.55), append=TRUE)
  
  # expect_equal(sum(names(gs$side.1) == 'axis'), 3)
  gs <- gsplot() %>% 
    points(0:1,0:1) %>% 
    axis(side=1, at=c(0.5,1)) %>% 
    axis(side=1, at=c(0.25, 0.75), append=TRUE) %>% 
    axis(side=1, at=c(0.45, 0.55), append=TRUE) %>% 
    axis(side=1, at=c(0.33))
  expect_equal(sum(names(gs$side.1) == 'axis'), 1)
  expect_equal(gs$side.1$axis$at, 0.33)
})

test_that("axis tracks append FALSE by default",{
  gs <- gsplot() %>% 
    points(0:1,0:1) %>% 
    axis(side=1, at=c(0.5,1)) %>% 
    axis(side=1, at=c(0.25, 0.75)) %>% 
    axis(side=1, at=c(0.45, 0.55), append=TRUE)
  # expect_equal(sum(names(gs$side.1) == 'axis'), 2)
})

context("axis style arguments handled appropriately")

test_that("par args sent to axis() end up in axis args",{
  gs <- points(gsplot(), 1, 0) %>% axis(side=1, tcl = -0.25)
  expect_equal(gs$side.1$axis[["tcl"]], -0.25)
})

test_that("special args given to axis are retained", {
  gs <- points(gsplot(), 1, 0) %>% axis(side=1, n.minor = 4)
  expect_equal(gs$side.1$axis[["n.minor"]], 4)
  
  gs <- points(gsplot(), 1, 0) %>% axis(side=1, tcl.minor = -0.136)
  expect_equal(gs$side.1$axis[["tcl.minor"]], -0.136)
})

test_that("style params given to points calls are in side par, style on axis stay there",{
  gs <- points(gsplot(), 1, 0, tcl=0.5) %>% axis(side=1, tcl = -0.136)
  expect_equal(gs$side.1$axis[["tcl"]], -0.136)
  expect_equal(gs$side.1$par[["tcl"]], 0.5)
})


context("axis user flipped on")

test_that("axis user FALSE by default",{
  expect_false(gsplot:::add_new_side(gsplot(), 'side.1')$side.1$usr.axes)
})

test_that("axis user flipped to TRUE when specified",{
  gs <- gsplot:::add_new_side(gsplot(), 'side.1')
  class(gs) <- 'gsplot'
  gs <- axis(gs, 1)
  expect_true(gs$side.1$usr.axes)
})



test_that("format",{
  
  gs <- gsplot() %>%
    points(seq.Date(as.Date("2010-01-01"), as.Date("2010-12-31"), by="month"),
           1:12) %>%
    axis(side = 1, format="%Y-%m")
  
  expect_true(class(gs$side.1$lim) == "Date")
  
  gs <- gsplot() %>%
    points(seq.Date(as.Date("2010-01-01"), as.Date("2010-12-31"), by="month"),
           1:12) %>%
    axis(side = 1, format="%B")
  expect_equal(gs$side.1$axis$format, "%B")
})