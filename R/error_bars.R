#' gsplot error bars
#'
#' Creates vertical and horizontal error bars around plot points. 
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#'
#' @rdname error_bar
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=c(3,1,2), x=1:3, xlim=c(0,NA),ylim=c(0,NA),
#'             col="blue", pch=18, legend.name="Points")
#' gsNew <- lines(gsNew, c(3,4,3), c(2,4,6), legend.name="Lines")
#' gsNew <- abline(gsNew, b=1, a=0, legend.name="1:1")
#' gsNew <- legend(gsNew, location = "topleft",title="Awesome!")
#' gsNew <- grid(gsNew)
#' gsNew <- error_bar(gsNew, 1:3, y=c(3,1,2), y.high=c(0.5,0.25,1), y.low=0.1)
#' gsNew <- error_bar(gsNew, x=1:3, y=c(3,1,2), x.low=c(.2,NA,.2), x.high=.2, col="red",lwd=3)
#' gsNew <- title(gsNew, "Graphing Fun")
#' gsNew
error_bar <- function(object, ...) {
  override("gsplot", "error_bar", object, ...)
}


error_bar.gsplot <- function(object, x, y, y.high=0, y.low=0, x.high=0, x.low=0, epsilon=0.1, ..., legend.name=NULL, side=c(1,2)){
  current_list <- config("error_bar")
  arguments <- list(...)
  arguments <- append(list(x=x, y=y, y.high=y.high, y.low=y.low, x.high=x.high, x.low=x.low, epsilon=epsilon),arguments)
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  object <- append(object,  list(error_bar = list(arguments = arguments, 
                                                 gs.config=list(legend.name = legend.name, 
                                                                side = side))))
  return(gsplot(object))
}


#' @rdname error_bar
#' @param x value of data point on the x-axis
#' @param y value of data point on the y-axis
#' @param y.low numeric lower y offset for error bar (this is subtracted from y)
#' @param y.high numeric upper y offset for error bar (this is added to y)
#' @param x.low numeric lower x offset for error bar (this is subtracted from x)
#' @param x.high numeric upper x offset for error bar (this is added to x)
#' @param epsilon numeric width of the bar
error_bar.default <- function(x, y, y.high=0, y.low=0, x.high=0, x.low=0, epsilon=0.1, ...){
  
  y.high[is.na(y.high)] <- 0
  y.low[is.na(y.low)] <- 0
  x.high[is.na(x.high)] <- 0
  x.low[is.na(x.low)] <- 0
  
  ep.y.low <- epsilon
  ep.y.high <- epsilon
  ep.x.low <- epsilon
  ep.x.high <- epsilon
  
  if(length(epsilon) == 1){
    ep.y.low <- rep(epsilon,length(y.low))
    ep.y.high <- rep(epsilon,length(y.high))
    ep.x.low <- rep(epsilon,length(x.low))
    ep.x.high <- rep(epsilon,length(x.high))
  } 
  
  ep.y.low[y.low == 0] <- 0
  ep.y.high[y.high == 0] <- 0
  ep.x.low[x.low == 0] <- 0
  ep.x.high[x.high == 0] <- 0
  
  if(!all(y.low == 0) && !all(y.high == 0)){
    segments(x, y-y.low,x, y+y.high, ...)
    segments(x-ep.y.low, y-y.low, x+ep.y.low,y-y.low, ...)
    segments(x-ep.y.high,y+y.high,x+ep.y.high,y+y.high, ...)    
  }

  if(!all(x.low == 0) && !all(x.high == 0)){
    segments(x-x.low, y, x+x.high, y, ...)
    segments(x-x.low, y-ep.x.low,x-x.low,y+ep.x.low, ...)
    segments(x+x.high, y-ep.x.high,x+x.high,y+ep.x.high, ...) 
  }

}



