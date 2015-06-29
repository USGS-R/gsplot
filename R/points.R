#' gsplot points
#'
#' Point stuff
#' @usage 
#' points(object, legend.name=NULL, side=c(1,2), ...)
#' 
#' @details Add additional functionality to points.
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return modified gsplot object 
#' @rdname points
#' @examples
#' gs <- gsplot(list())
#' gsNew <- points(gs, 1, 2, col="blue", pch=18)
#' gsNew <- points(gsNew, c(3,4,3), c(2,4,6))
#' gsNew
#' @export
points <- function(object, ...) {
  overrideGraphics("points", object, ...)
}


points.gsplot <- function(object, x, y=NULL, ..., legend.name=NULL, side=c(1,2)){
  current_list <- config("points")
  arguments <- list(x=x, y=y, ...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  object <- append(object,  list(points = list(arguments = arguments, 
                                            gs.config=list(legend.name = legend.name, 
                                            side = side))))
  return(gsplot(object))
}