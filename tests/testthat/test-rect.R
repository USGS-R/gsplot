context("test rect")

test_that("testing rectangle arguments for limits", {
  
  gs <- gsplot()  %>% 
    rect(1,5,2,7, col="pink", border=NA) %>%
    points(1:5, 1:5, pch=5, col="blue") %>%
    lines(3:7, 1:5, col="darkgreen") %>%
    error_bar(x=4, y=4, y.high=0.5, y.low=0.7, epsilon=0.5) 

  expect_equal(gs$side.2$lim, c(1,7))
  
  gs <- gsplot()  %>% 
    rect(-1,5,2,7, col="pink", border=NA) %>%
    points(1:5, 1:5, pch=5, col="blue") %>%
    lines(3:7, 1:5, col="darkgreen") %>%
    error_bar(x=4, y=4, y.high=0.5, y.low=0.7, epsilon=0.5) 
  
  expect_equal(gs$side.1$lim, c(-1,7))
})