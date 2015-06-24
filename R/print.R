#' gsplot show
#'
#' show gsplot
#'
#' @param x gsplot object
#' @param \dots stuff
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot(list())
#' gsNew <- points(gs, x=1, y=2)
#' gsNew
print.gsplot <- function(x, ...){
  
  # -- set plot -- 
  plot(x=0,ylim=c(0,10),xlim=c(0,10))
  
  # -- call lines -- 
  to_gsplot(x, which(names(x)  %in% 'lines'))

  # -- call points -- 
  to_gsplot(x, which(names(x)  %in% 'points'))
  
  print("oh my")
}

to_gsplot <- function(x, which_i){
  for (i in which_i){
    do.call(names(x[i]),x[[i]])
  }
}