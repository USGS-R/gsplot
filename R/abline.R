#' gsplot abline
#'
#' Creates straight lines on the existing gsplot object.  
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @details Additional graphical parameter inputs:
#' \itemize{
#'  \item{\code{a, b}} {slope, y-intercept}
#'  \item{\code{h}} {the y-value specifying a horizontal line}
#'  \item{\code{v}} {the x-value specifying a vertical line}
#'  \item{\code{col, lty, lwd}} {parameters describing the color, type, and width of the line, respectively}
#'  \item{\code{legend.name}} {name that appears the legend, see \code{\link{legend}}}
#'  } 
#'
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, col="blue", pch=18, legend.name="Points")
#' gsNew <- lines(gsNew, c(3,4,3), c(2,4,6), legend.name="Lines")
#' gsNew <- abline(gsNew, b=1, a=0, legend.name="1:1")
#' gsNew <- legend(gsNew, "topleft",title="Awesome!")
#' gsNew
abline <- function(object, ...) {
  override("graphics", "abline", object, ...)
}


abline.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  current_list <- config("abline")
  arguments <- list(...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  object <- append(object,  list(abline = list(arguments = arguments, 
                                               gs.config=list(legend.name = legend.name, 
                                                              side = side))))
  return(gsplot(object))
}