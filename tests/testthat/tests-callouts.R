context("test callouts")
test_that("lines to callouts", {
  
  g = callouts(gsplot(), c(0,3), NULL, labels=c('dogs','cats'))
  expect_warning(print(g))
  callouts(gsplot(), c(0,3), y=1:2, labels=c('dogs','cats'), angle=30)
  callouts(gsplot(), c(0,3), y=1:2, labels=c('dogs','cats'), angle='auto')
  
})

test_that("testing callout config", {
  
  gsp <- gsplot(config.file = system.file("extdata", "lineScatter.yaml", package = "gsplot")) %>%
    points(y=1, x=2, xlim=c(0,3),ylim=c(0,3),
           col="blue", pch=18, legend.name="Points") %>%
    callouts(2, 1, labels='dog')
  
  expect_true(gsp$global$config$config.file)
  expect_equal(gsp$view.1.2$callouts$col, "red")
  
  gsp <- gsplot() %>%
    points(y=1, x=2, xlim=c(0,3),ylim=c(0,3),
           col="blue", pch=18, legend.name="Points") %>%
    callouts(2, 1, labels='dog')
  
  expect_false(gsp$global$config$config.file)
  expect_equal(gsp$view.1.2$callouts$col, "black")
  
})