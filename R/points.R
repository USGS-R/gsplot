#' gsplot points
#'
#' Creates points on gsplot. 
#' 
#' @details Additional graphical parameter inputs:
#' \itemize{
#'  \item{\code{x}} {vector indicating the x-coordinate(s) of the plot point(s)}
#'  \item{\code{y}} {vector indicating the y-coordinate(s) of the plot point(s)}
#'  \item{\code{xlim}} {vector containing the lower and upper limit for the x-axis}
#'  \item{\code{ylim}} {vector containing the lower and upper limit for the y-axis}
#'  \item{\code{col, pch}} {parameters describing the color and type of point, respectively}
#'  \item{\code{legend.name}} {name that appears the legend, see \code{\link{legend}}}
#'   }
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, col="blue", pch=18)
#' gsNew <- points(gsNew, c(3,4,3), c(2,4,6), ylim=c(0,10))
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