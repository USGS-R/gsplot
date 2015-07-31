#' gsplot lines
#'
#' Creating a line by specifying specific plot points. 
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @details Additional graphical parameter inputs:
#' \itemize{
#'  \item{\code{x}} {vector of x-coordinates for points that make up the line}
#'  \item{\code{y}} {vector of y-coordinates for points that make up the line}
#'  \item{\code{legend.name}} {a character vector of length one to be used in the legend}
#'  \item{\code{side}} {vector specifying the side(s) to use for axes (1,2,3,4 for sides, or 5,6,7,8 for outward offsets of those)}
#' }  
#'    
#' @examples
#' gsNew <- gsplot()
#' gsNew <- lines(gsNew, c(1,2), y=c(2,5))
#' gsNew <- lines(gsNew, c(3,4,3), c(2,4,6), pch=6)
#' gsNew <- points(gsNew, c(8,4,1.2), c(2,4.7,6), side=c(3,2))
#' gsNew
#' 
#' # Same example using the magrittr pipe '%>%' to connect operations within gsplot
#' gsNewpipe <- gsplot() %>%
#'    lines(c(1,2), c(2,5)) %>%
#'    lines(c(3,4,3), c(2,4,6), pch=6) %>%
#'    points(c(8,4,1.2), c(2,4.7,6), side=c(3,2))
#' gsNewpipe
#' @export
#' @rdname lines
lines <- function(object, ...) {
  override("graphics", "lines", object, ...)
}


lines.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  fun.name <- "lines"
  to.gsplot <- list(list(arguments = set_args(fun.name, ...), 
                         gs.config=list(legend.name = legend.name, side = side))) %>% 
    setNames(fun.name)
  return(gsplot(append(object, to.gsplot)))
}