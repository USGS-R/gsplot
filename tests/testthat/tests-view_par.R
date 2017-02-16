context("view par")
test_that("par arguments", {
  
  arguments = list(x=3, y=34, col='yellow',las=3)
  expect_equal(gsplot:::formal_arguments(arguments, def.funs=c(graphics::plot.xy, graphics::points.default)), list(x=3, y=34, col='yellow'))
  
})

test_that("par within view", {
  
  gs = points(gsplot(), y=1, x=2, col="blue", pch=18, las=3)  %>% lines(2:3,4:5)  %>% points(3,4,side=c(3,4),las=1)
  expect_equal(gs[["view.1.2"]]$points$col, 'blue')
  expect_equal(gs[["side.1"]]$par$las, 3)
  expect_equal(gs[["side.2"]]$par$las, 3)
  expect_equal(gs[["view.3.4"]]$points$col, 'red') # default
  expect_equal(gs[["side.3"]]$par$las, 1)
  expect_equal(gs[["side.4"]]$par$las, 1)
  
  gs = points(gsplot(), y=1, x=2, col="blue", pch=18, las=3) %>% 
    lines(2:3,4:5) %>% 
    points(3,4,side=c(3,4),las=1, ann=FALSE)
  
})

test_that("view_info", {
  
  gs = points(gsplot(), y=1, x=2, col="blue", pch=18, las=3)  %>% lines(2:3,4:5)  %>% points(3,4,side=c(3,4),las=1)
  view.information <- view_info(gs)
  expect_equal(nrow(view.information), 2)
  expect_equal(view.information$log, c("",""))
  
  usrDef <- gsplot(mar=c(4,4,4,4)) %>% 
    points(x=1, y=2, side=c(3,2), cex=3, xlab='cat',log='x') %>% 
    points(x=3:10,y=4:11, side=c(1,2), log='y')
  
  view.information <- view_info(usrDef)
  expect_equal(nrow(view.information), 2)
  # expect_equal(view.information$log, c("xy","y"))
  
})