#' gsplot segments
#'
#' segments stuff
#' 
#' @details Add additional functionality to points.
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, xlim=c(0,NA),ylim=c(0,NA),
#'             col="blue", pch=18, legend.name="Points")
#' gsNew <- lines(gsNew, c(3,4,3), c(2,4,6), legend.name="Lines")
#' gsNew <- abline(gsNew, b=1, a=0, legend.name="1:1")
#' gsNew <- legend(gsNew, "topleft",title="Awesome!")
#' gsNew <- grid(gsNew)
#' gsNew <- segments(gsNew, x0=2, y0=0.75, y1=1.25)
#' gsNew
segments <- function(object, ...) {
  overrideGraphics("segments", object, ...)
}


segments.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  current_list <- config("segments")
  arguments <- list(...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  object <- append(object,  list(segments = list(arguments = arguments, 
                                             gs.config=list(legend.name = legend.name, 
                                                            side = side))))
  return(gsplot(object))
}