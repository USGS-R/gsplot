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
  arguments <- append(list(x=x, y=y, labels=labels, length=length, angle=angle), arguments)
  arguments <- append(arguments, current_list[indicesToAdd]) 
  
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
  
  stopifnot(angle >= 0, angle <= 360)
  # // to do: possibly support angle and length as vectors equal in length to x 
  x.usr <- par("usr")[c(1,2)]
  if (par("xlog"))
    x.usr <- 10^x.usr
  y.usr <- par("usr")[c(3,4)]
  if (par("xlog"))
    y.usr <- 10^y.usr

  xrange <- diff(x.usr)
  yrange <- diff(y.usr)
  x1 = x + length * xrange * cos(2*pi*(angle/360));
  y1 = y + length * yrange * sin(2*pi*(angle/360));
  if (angle >= 315 | angle <= 45){
    pos = 4
  } else if (angle > 45 & angle <= 135) {
    pos = 3
  } else if (angle > 135 & angle <= 225){
    pos = 2
  } else {
    pos = 1
  }
  
  segments(x0=x, y0=y, x1=x1, y1=y1, ...)
  text(x=x1, y=y1, labels=labels, pos=pos,...)
  
}