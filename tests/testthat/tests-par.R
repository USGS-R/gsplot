context("par")

test_that("lists and named args are identical", {
  gs <- gsplot(cex=1.2) %>% 
    par(cex=3.2)
  expect_equal(gs[['par']][['cex']],3.2)
  gs <- gsplot(cex=1.2)
  expect_equal(gs[['par']][['cex']],1.2)
  
})

test_that("a more complicated par set", {
  gs <- gsplot(cex=1.2) %>% 
    par(cex=3.2) %>%
    par(cex=3.8,mar=c(4,4,4,4)) %>% 
    points(3,2,cex=2)
  expect_equal(gs[['par']][['cex']],3.8)
})

test_that("par is a list",{
  gs <- gsplot(cex=1.2)
  expect_is(gs[['par']], 'list')
})