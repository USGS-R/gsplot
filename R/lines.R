#' gsplot lines
#'
#' Creating a line by specifying specific plot points.  See \code{\link[graphics]{lines}} for more details. 
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
#'  \item{\code{error_bar}} {add error bars to the defined line, see \code{\link{error_bar}} 
#'  for arguments, must add arguments as a list}
#'  \item{\code{callouts}} {add callouts and text to the defined line, see \code{\link{callouts}} 
#'  for arguments, must add arguments as a list}
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
#' 
#' gs <- gsplot() %>%
#'    lines(x=c(1,2), y=c(4,2), xlim=c(0, 5), ylim=c(0,5),
#'        callouts=list(labels=c(NA, "data"), col="blue"))
#' gs
#' @export
#' @rdname lines
lines <- function(object, ...) {
  override("graphics", "lines", object, ...)
}


lines.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  fun.name <- "lines"
  arguments <- list(...)
  
  if (is.null(names(arguments))){
    arguments_gsplot <- arguments
  } else {
    arguments_gsplot <- arguments[!names(arguments) %in% c("callouts", "error_bar")]
  }
  
  to.gsplot <- list(list(arguments = do.call(set_args, c(fun.name, arguments_gsplot)),  
                         gs.config=list(legend.name = legend.name, side = side))) %>% 
    setNames(fun.name)
  
  if (all(names(to.gsplot$lines$arguments) != "formula") && is.null(to.gsplot$lines$arguments$y)){
    to.gsplot$lines$arguments$y <- to.gsplot$lines$arguments$x
    to.gsplot$lines$arguments$x <- seq(length(to.gsplot$lines$arguments$x))
    if (is.null(to.gsplot$lines$arguments$xlab)) to.gsplot$lines$arguments$xlab <- "Index" 
  }
  
  if ("callouts" %in% names(arguments)){
    object <- callouts(object, x=to.gsplot$lines$arguments$x, 
                       y=to.gsplot$lines$arguments$y, arguments$callouts)
  }
  if ("error_bar" %in% names(arguments)){
    object <- error_bar(object, x=to.gsplot$lines$arguments$x, 
                        y=to.gsplot$lines$arguments$y, arguments$error_bar)
  }
  
  return(gsplot(append(object, to.gsplot)))
}