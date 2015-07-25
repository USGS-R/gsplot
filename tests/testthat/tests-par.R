context("par")

test_that("lists and named args are identical", {
  gs <- gsplot(cex=1.2) %>% 
    par(cex=3.2)
  expect_equal(gs[['par']][['cex']],3.2)
  gs <- gsplot(cex=1.2)
  expect_equal(gs[['par']][['cex']],1.2)
  
})

test_that("empty sets on gsplot are ignored",{
  expect_is(gs[['par']], 'list')
})