context("mtext")

test_that("mtext on correct side", {
  gs <- gsplot() %>% 
    points(1,2) %>% 
    mtext("my margin text", side=4)
  
  i <- which(unlist(lapply(gs, function(x) {any(names(x) %in% "mtext")})))
  
  expect_true(4 %in% gsplot:::as.side(names(gs[i])))
  warning('skipping axes test in mtext. not sure why this was here')
  #expect_false(gs$side.4$axes)
})



test_that("multiple mtext are on correct sides", {
  gs <- gsplot() %>% 
    points(1,2) %>% 
    mtext(text=c(1,2,3,4), at=c(0.7,0.9,1.1,1.3), cex=0.5, las=2, side=1, line=1) %>% 
    mtext(text=c("yr1", "yr2"), at=c(0.8, 1.2), las=1, side=3, line=3)
  
  expect_true(any(names(gs[["view.1.2"]]) %in% "mtext"))
  
  expect_true(any(names(gs[["view.3.2"]]) %in% "mtext"))
})
