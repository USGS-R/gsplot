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
#' gsNew <- legend(gsNew, "topleft",title="Awesome!")
#' gsNew <- grid(gsNew)
#' gsNew <- error_bar(gsNew, 1:3, y=c(3,1,2), y.high=c(0.5,0.25,1), y.low=0.1)
#' gsNew <- error_bar(gsNew, x=1:3, y=c(3,1,2), x.low=.2, x.high=.2, col="red",lwd=3)
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
#' @param y.low numeric lower offset for error bar (this is subtracted from y)
#' @param y.high numeric upper offset for error bar (this is added to y)
error_bar.default <- function(x, y, y.high=0, y.low=0, x.high=0, x.low=0, epsilon=0.1, ...){
  
  if(!all(is.na(y.high)) & !all(is.na(y.low))){
    segments(x, y-y.low,x, y+y.high, ...)
    segments(x-epsilon,y-y.low,x+epsilon,y-y.low, ...)
    segments(x-epsilon,y+y.high,x+epsilon,y+y.high, ...)    
  }
  
  if(!all(is.na(x.high)) & !all(is.na(x.low))){
    segments(x-x.low, y, x+x.high, y, ...)
    segments(x-x.low, y-epsilon,x-x.low,y+epsilon, ...)
    segments(x+x.high, y-epsilon,x+x.high,y+epsilon, ...)    
  }

  
}



