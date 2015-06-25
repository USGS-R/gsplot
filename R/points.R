#' gsplot points
#'
#' Point stuff
#'
#' @param object gsplot object
#' @param legend.name character
#' @param side integer vector
#' @param \dots ...	Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot(list())
#' gsNew <- points(gs, x=1, y=2)
#' gsNew <- points(gsNew, x=c(3,4,3), y=c(2,4,6))
points <- function(object, legend.name=NULL, side=c(1,2), ...){
  
  if (!missing(object) && class(object) == "gsplot" ){
    object <- append(object, list(points = list(..., legend.name = legend.name, side = side)))
    return(gsplot(object))
  } else {
    if (missing(object)){
      graphics::points(...)
    } else {
      graphics::points(object, ...)
    }
  }
}