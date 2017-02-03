context("theme")

test_that("theme",{
  
gs <- gsplot(theme = theme.hadley) %>%
       points(1:10, 1:10, xlab="Index")
expect_length(gs, 5)
  
})