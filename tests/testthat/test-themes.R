context("theme")

test_that("theme",{
  
gs <- gsplot(theme = theme.hadley) %>%
       points(1:10, 1:10, xlab="Index")
expect_length(gs, 6)
expect_true(gs$global$config$config.file)
expect_equal(gs$view.1.2$points$col, "brown")

  
})