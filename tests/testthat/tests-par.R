context("par")

test_that("lists and named args are identical", {
  gs <- gsplot(cex=1.2) %>% 
    par(cex=3.2)
  expect_equal(gs[['global']][['par']][['cex']],3.2)
  gs <- gsplot(cex=1.2)
  expect_equal(gs[['global']][['par']][['cex']],1.2)
  
})

test_that("a more complicated par set", {
  gs <- gsplot(cex=1.2) %>% 
    par(cex=3.2) %>%
    par(cex=3.8,mar=c(4,4,4,4)) %>% 
    points(3,2,cex=2)
  expect_equal(gs[['global']][['par']][['cex']],3.8)
})

test_that("par is a list",{
  gs <- gsplot(cex=1.2)
  expect_is(gs[['global']][['par']], 'list')
  expect_is(par(), 'list')
})

test_that("graphics par behaves as expected",{
  expect_is(par("usr")[c(1,2)], 'numeric')
  expect_equal(par("usr")[c(1,2)],par()$usr[c(1,2)])
  expect_equal(gsplot::par("usr","mar"), graphics::par("usr","mar"))
})