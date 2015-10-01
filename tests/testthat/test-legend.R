context("legend")

#test_that("graphics examples work", {
#  plot(-4:4, -4:4, type = "n")  # setting up coord. system
#  lines(c(0,0), y=c(2,5), col="yellow")
#  lines(c(3,4,3), c(2,4,6), col="blue")
#  points(rnorm(200), rnorm(200), col = "green", pch=3)
#  points(rnorm(200), rnorm(200), col = "red", pch=7)
#  legend(location="bottom")
#  bottom
        
#})

test_that("gsplot legend works", {
  
  gs <- gsplot() 
  
  expect_is(gs, "gsplot")
  
  gs <- points(gs, x=1, y=2, side=c(3,2), legend.name="Points 1", pch=1, col="blue") %>% 
    points(x=3, y=4, side=c(1,4), legend.name="Points 2", pch=5, col="red") %>%  
    legend(location="bottomright")

  expect_equal(gs$legend$gs.config$location, "bottomright")
  
})

test_that("gsplot legend args are still the same", {
  
  overall.legend <- c("x", "y", "bty", "bg", "box.lty", "box.lwd", "box.col", "cex",
                      "xjust", "yjust", "x.intersp", "y.intersp", "adj", "text.width", 
                      "merge", "trace", "plot", "ncol", "horiz", "title", "inset", 
                      "xpd", "title.col", "title.adj", "seg.len")  
  
  fun.specific <- c("legend", "fill", "col", "border", "lty", "lwd", "pch", "angle", 
                    "density", "pt.bg", "pt.cex", "pt.lwd", "text.col", "text.font")
  
  all.formals <- names(formals(graphics::legend))

  expect_equal(length(all.formals[!all.formals %in% overall.legend]), length(fun.specific)) 
  
  expect_equal(length(all.formals[!all.formals %in% fun.specific]), length(overall.legend)) 
  
})

