context("config")

cleanup <- function() {
  loadConfig()
}

test_that("lists and named args are identical", {
  expect_equal(gsplot:::config("par",list("mar"=c(1,2,3,4)))$mar, c(1,2,3,4))
  expect_equal(gsplot:::config("par",mar=c(1,2,3,4))$mar, c(1,2,3,4))
})

test_that("empty sets on gsplot are ignored",{
  expect_is(gsplot:::config("par"), 'list')
})

test_that("persisting to config alters environment", {
  gsplot:::config("par", tcl=0.5, persist=TRUE)
  gsplot:::config("points", col="blue", persist=TRUE)
  expect_equal(gsplot:::config("par")$tcl, 0.5)
  expect_equal(gsplot:::config("points")$col, "blue")
  cleanup()
})

test_that("formals are correctly retrieved", {
  expect_equal(length(config("par")), 7)
  expect_equal(names(config("points")), c("pch", "col"))
  expect_equal(length(config("arrows")), 0)
})

test_that("non-existant type hits error", {
  expect_error(config("foo"))
})

test_that("config temp", {
  orig.par <- par(no.readonly = TRUE)
  df <- data.frame(x = 1:10, y=1:10, z = seq(2,20,2))
  
  gsp <- gsplot(config.file = system.file("extdata", "lineScatter.yaml", package = "gsplot")) %>%
    lines(df$x, df$y, col="red", legend.name = "points") %>%
    legend()
  expect_true(gsp$global$config$config.file)
  expect_equal(par(no.readonly = TRUE)$lwd, orig.par$lwd)
  print(gsp)
  expect_equal(par(no.readonly = TRUE)$lwd, 0.8)
  
  gspDef <- gsplot() %>%
    lines(df$x, df$y, col="red", legend.name = "points") 
  expect_false(gspDef$global$config$config.file)
  expect_equal(par(no.readonly = TRUE)$lwd, orig.par$lwd)
  
  loadConfig(system.file("extdata", "lineScatter.yaml", package = "gsplot"))
  expect_equal(par(no.readonly = TRUE)$lwd, orig.par$lwd)
  
  gsp <- gsplot() %>%
    lines(df$x, df$y, col="red", legend.name = "points") 
  expect_equal(par(no.readonly = TRUE)$lwd, orig.par$lwd)
  expect_false(gsp$global$config$config.file)
  expect_equal(gsp$side.1$axis$lwd, 0.8)
  print(gsp)
  expect_equal(par(no.readonly = TRUE)$lwd, 0.8)
  
  loadConfig()
  expect_equal(par(no.readonly = TRUE)$lwd, orig.par$lwd)
  gspDef2 <- gsplot() %>%
    lines(df$x, df$y, col="red", legend.name = "points") 
  expect_false(gspDef2$global$config$config.file)
  expect_null(gspDef2$side.1$axis$lwd)
})
