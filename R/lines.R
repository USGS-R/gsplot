#' gsplot lines
#'
#' Point stuff
#' @param object a gsplot object
#' @param legend.name a character vector of length one to be used in the legend (if needed)
#' @param side the side(s) to use for axes (1,2,3,4 for sides, or 5,6,7,8 for outward offsets of those)
#' @param \dots ...	Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return modified gsplot object 
#' @examples
#' gs <- gsplot(list())
#' gsNew <- lines(gs, x=1, y=2)
#' gsNew <- lines(gsNew, x=c(3,4,3), y=c(2,4,6))
#' gsNew <- points(gsNew, x=c(8,4,1.2), y=c(2,4.7,6), side=c(3,2))
#' gsNew
#' @export
lines <- function(object, ...) {
  overrideGraphics("lines", object, ...)
}


lines.gsplot <- function(object, legend.name=NULL, side=c(1,2), ...){
  
  object <- append(object, list(lines = list(..., legend.name = legend.name, side = side)))
  return(gsplot(object))
}