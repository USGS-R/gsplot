#' gsplot segments
#'
#' error_bar_vertical stuff
#' 
#' @details Add additional functionality to points.
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return modified gsplot object 
#' @rdname error_bar
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=c(3,1,2), x=1:3, xlim=c(0,NA),ylim=c(0,NA),
#'             col="blue", pch=18, legend.name="Points")
#' gsNew <- lines(gsNew, c(3,4,3), c(2,4,6), legend.name="Lines")
#' gsNew <- abline(gsNew, b=1, a=0, legend.name="1:1")
#' gsNew <- legend(gsNew, "topleft",title="Awesome!")
#' gsNew <- grid(gsNew)
#' gsNew <- error_bar_vertical(gsNew, x=1:3, y=c(3,1,2), y.high=c(0.5,0.25,1), y.low=0.1)
#' gsNew <- error_bar_horizontal(gsNew, x=1:3, y=c(3,1,2), x.low=.2, x.high=.2, col="red",lwd=3)
#' gsNew <- title(gsNew, "Graphing Fun")
#' gsNew
error_bar_vertical <- function(object, ...) {
  overrideGraphics("error_bar_vertical", object, ...)
}


error_bar_vertical.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  current_list <- config("error_bar_vertical")
  arguments <- list(...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  object <- append(object,  list(error_bar_vertical = list(arguments = arguments, 
                                                 gs.config=list(legend.name = legend.name, 
                                                                side = side))))
  return(gsplot(object))
}

#' @export
#' @rdname error_bar
#' @param x numeric
#' @param y numeric
#' @param y.low numeric lower offset for error bar (this is subtracted from y)
#' @param y.high numeric upper offset for error bar (this is added to y)
error_bar_vertical.default <- function(x, y, y.high, y.low, epsilon=0.1, ...){
  
  segments(x, y-y.low,x, y+y.high, ...)
  segments(x-epsilon,y-y.low,x+epsilon,y-y.low, ...)
  segments(x-epsilon,y+y.high,x+epsilon,y+y.high, ...)
  
}

#' @export
#' @rdname error_bar
error_bar_horizontal <- function(object, ...) {
  overrideGraphics("error_bar_horizontal", object, ...)
}

error_bar_horizontal.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  current_list <- config("error_bar_horizontal")
  arguments <- list(...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  object <- append(object,  list(error_bar_horizontal = list(arguments = arguments, 
                                                           gs.config=list(legend.name = legend.name, 
                                                                          side = side))))
  return(gsplot(object))
}

#' @export
#' @rdname error_bar
#' @param epsilon half-width of error bar edge
#' @param x.low numeric lower offset for error bar (this is subtracted from x)
#' @param x.high numeric upper offset for error bar (this is added to x)
error_bar_horizontal.default <- function(x, y, x.high, x.low, epsilon=0.1, ...){
  
  segments(x-x.low, y, x+x.high, y, ...)
  segments(x-x.low, y-epsilon,x-x.low,y+epsilon, ...)
  segments(x+x.high, y-epsilon,x+x.high,y+epsilon, ...)
  
}

