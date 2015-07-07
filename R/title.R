#' gsplot title
#'
#' title stuff
#' 
#' @details Add additional functionality to title.
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, col="blue", pch=18, legend.name="Points", xlab="Stuff")
#' gsNew <- lines(gsNew, c(3,4,3), c(2,4,6), legend.name="Lines", ylab="Data!")
#' gsNew <- abline(gsNew, b=1, a=0, legend.name="1:1")
#' gsNew <- legend(gsNew, "topleft",title="Awesome!")
#' gsNew <- title(gsNew, main="Great Graph")
#' gsNew
title <- function(object, ...) {
  overrideGraphics("title", object, ...)
}


title.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  current_list <- config("title")
  arguments <- list(...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  object <- append(object,  list(title = list(arguments = arguments, 
                                               gs.config=list(legend.name = legend.name, 
                                                              side = side))))
  return(gsplot(object))
}