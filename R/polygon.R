#' gsplot polygon
#'
#' Plot a polygon by specifying vertices. 
#'
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @details Additional graphical parameter inputs: 
#' \itemize{
#'  \item{\code{x}} {vector specifying the x values of the vertices}
#'  \item{\code{y}} {vector specifying the y values of the vertices}
#'  \item{\code{density}} {density of shading lines (lines per inch), NULL means no shading, NA suppresses shading and allows color fill}
#'  \item{\code{angle}} {angle of shading lines (degrees)}
#'  \item{\code{col}} {shade or fill color(s), NA means no fill (transparent)}
#'  \item{\code{border}} {color of border, NA means no border, TRUE indicates the same color as shading lines}
#'  \item{\code{lty}} {line type for borders and shading}
#'  \item{\code{lwd}} {line width for borders and shading}
#'  }
#'    
#' @rdname polygon
#' @export
#' @examples
#' gs <- gsplot() %>%
#'    polygon(x=c(1,2,5), y=c(1,4,0.5), 
#'            density=10, col="darkgreen")
#' gs
#' 
#' gs <- gsplot() %>%
#'    polygon(x=c(2,6.5,12,10.5,14,9,6.5,5,0,3.5),
#'            y=c(0,2.5,0,5,8.5,9,14,9,8.5,5),
#'            density=NA, col="blue", border=NA) 
#' gs
polygon <- function(object, ...) {
  override("graphics", "polygon", object, ...)
}

polygon.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  
  current_list <- config("polygon")
  arguments <- list(...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  object <- append(object,  list(polygon = list(arguments = arguments, 
                                              gs.config=list(legend.name = legend.name, 
                                                             side = side))))
  
  return(gsplot(object))
  
}