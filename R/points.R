#' gsplot points
#'
#' Point stuff
#' @usage 
#' points(object, legend.name=NULL, side=c(1,2), ...)
#' 
#' @param object gsplot object
#' @param legend.name character
#' @param side integer vector
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return modified gsplot object 
#' @examples
#' gs <- gsplot(list())
#' gsNew <- points(gs, x=1, y=2, col="blue", pch=18)
#' gsNew <- points(gsNew, x=c(3,4,3), y=c(2,4,6))
#' 
#' @export
points <- function(object, ...) {
  overrideGraphics("points", object, ...)
}

points.gsplot <- function(object, legend.name=NULL, side=c(1,2), ...){
  current_list <- config("points")
  arguments <- list(...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  object <- append(object,  list(points = list(arguments = arguments, 
                                            gs.config=list(legend.name = legend.name, 
                                            side = side))))
  return(gsplot(object))
}