#' gsplot rect
#'
#' Add single or multiple rectangles to the plotting region.  See \code{\link[graphics]{rect}} for more details.
#'
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @details Additional graphical parameter inputs: 
#' \itemize{
#'  \item{\code{xleft}} {vector or scalar indicating left positions on the x-axis}
#'  \item{\code{xright}} {vector or scalar indicating right positions on the x-axis}
#'  \item{\code{ybottom}} {vector or scalar indicating bottom positions on the y-axis}
#'  \item{\code{ytop}} {vector or scalar indicating top positions on the y-axis}
#'  \item{\code{density}} {density of shading lines (lines per inch), NULL means no shading, NA suppresses shading and allows color fill}
#'  \item{\code{angle}} {angle of shading lines (degrees)}
#'  \item{\code{col}} {shade or fill color(s), NA means no fill (transparent)}
#'  \item{\code{border}} {color of border, NA means no border, TRUE indicates the same color as shading lines}
#'  \item{\code{lty}} {line type for borders and shading}
#'  \item{\code{lwd}} {line width for borders and shading}
#'  }
#'    
#' @rdname rect
#' @export
#' @examples
#' gs <- gsplot() %>%
#'    points(x=c(1:5, 3.5), y=c(1:5, 6), legend.name="Stuff") %>%
#'    lines(x=2:6, y=2:6, ylim=c(0,10)) %>%
#'    axis(side=c(1,2),labels=TRUE) %>%
#'    legend("topright") %>%
#'    rect(xleft=3.4, xright=3.6, ybottom=5, 
#'         ytop=7, density=NULL, border='purple', 
#'         lty=2, lwd=3)
#' gs
#' 
#' gs <- gsplot() %>%
#'    lines(x=10:20, y=c(10:15, 25, 17:20), 
#'          xlim=c(0,30), ylim=c(0,30), col='darkgreen', 
#'          legend.name="Some data") %>%
#'    rect(xleft=15, xright=17, ybottom=21, ytop=27, 
#'          density=10, angle=130, col='darkblue') %>%
#'    legend()
#' gs
rect <- function(object, ...) {
  override("graphics", "rect", object, ...)
}

rect.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  object <- set_window_args(object, fun.name="rect", ..., legend.name=legend.name, side=side, def.funs=graphics::rect)
  current.args <- object$view[[which(names(object$view) %in% 'window') - 1]]
  set_legend_args(object, fun.name='rect', current.args)
}

