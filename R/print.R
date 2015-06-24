#' gsplot show
#'
#' show gsplot
#'
#' @param x gsplot object
#' @param \dots stuff
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot(list(plot=list(x=0,ylim=c(0,10),xlim=c(0,10))))
#' gsNew <- points(gs, x=1, y=2)
#' gsNew
print.gsplot <- function(x, ...){
  #lapply(x, function(x) do.call(x, x[[]]))
  for (i in 1:length(x)){
    do.call(names(x[i]),x[[i]])
  }
  print("oh my")
}