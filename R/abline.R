#' gsplot abline
#'
#' abline stuff
#' 
#' @details Add additional functionality to points.
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return modified gsplot object 
#' @export
abline <- function(object, ...) {
  overrideGraphics("abline", object, ...)
}


abline.gsplot <- function(object, x, y=NULL, ..., legend.name=NULL, side=c(1,2)){
  current_list <- config("abline")
  arguments <- list(x=x, y=y, ...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  object <- append(object,  list(points = list(arguments = arguments, 
                                               gs.config=list(legend.name = legend.name, 
                                                              side = side))))
  return(gsplot(object))
}