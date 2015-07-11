#' gsplot callouts
#'
#' callouts stuff
#' 
#' @details Add additional functionality to callouts.
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, xlim=c(0,3),ylim=c(0,3),
#'             col="blue", pch=18, legend.name="Points")
#' gsNew <- callouts(gsNew, 2, 1, labels='dog')
#' gsNew
callouts <- function(object, ...) {
  overrideGraphics("callouts", object, ...)
}


callouts.gsplot <- function(object, x, y, labels=NA, length=0.1, angle=30, ..., side=c(1,2)){
  current_list <- config("callouts")
  arguments <- list(...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(list(x=x, y=y, labels=labels, length=length, angle=angle), 
                      arguments) #, current_list[indicesToAdd]) ???
  # // to do: figure out why I can't include current_list...
  
  
  object <- append(object,  list(callouts = list(arguments = arguments, 
                                             gs.config=list(side = side))))
  return(gsplot(object))
}
#' default for adding callouts to a plot
#' 
#' add callout arrows and text to a plot
#' 
#' @param x values for callout location
#' @param y values for callout location
#' @param labels text to be added to callout
#' @param length relative (percentage of window width and height) distance for callout
#' @param angle callout line angle
#' 
#' 
#' @keywords internal
#' @export
callouts.default <- function(x, y, labels, length, angle, ...){
  
  # // to do: use angle actually for x1 and y1
  # // to do: use angle to figure out what `pos` value should be 
  xrange <- diff(par("usr")[c(1,2)])
  yrange <- diff(par("usr")[c(3,4)])
  x1 <- x-xrange*length
  y1 <- y+yrange*length
  
  segments(x0=x, y0=y, x1=x1, y1=y1, ...)
  text(x=x1, y=y1, labels=labels, pos=2,...)
  
}