context("title")

test_that("testing content of gsplot list for title", {
  
  gs <- gsplot(list())
  
  expect_is(gs,"gsplot")
  
  gs <- gsplot()  %>% 
    points(1:10, 1:10)  %>%
    lines(20:30,20:30, side=c(3,4)) %>%
    title("Great Graph")
  
  expect_true(any(names(gs$global) %in% "title"))
  
})

