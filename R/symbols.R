#' gsplot symbols
#'
#' Add single or multiple symbolsangles to the plotting region.  See \code{\link[graphics]{symbols}} for more details.
#'
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @details Additional graphical parameter inputs: 
#' \itemize{
#'  \item{\code{x}} {vector indicating position(s) of the symbol(s) on the x-axis}
#'  \item{\code{y}} {vector indicating position(s) of the symbol(s) on the y-axis}
#'  \item{\code{circles}} {vector of the radii of the circle(s)}
#'  \item{\code{squares}} {vector of the length of the sides of the square(s)}
#'  \item{\code{rectangles}} {matrix with two columns: width(s) and height(s) of the rectangle(s)}
#'  \item{\code{stars}} {matrix of at least three columns with lengths of the rays from the center 
#'  of the star(s)}
#'  \item{\code{thermometers}} {matrix with three (or four) columns indicating width (column 1), 
#'  height (column 2), and proportion filled (column 3) of the thermometers}
#'  \item{\code{...}} {see \code{\link[graphics]{symbols}} for more details on graphical parameters}
#'  }
#'    
#' @rdname symbols
#' @export
#' @examples
#' 
#' gs <- gsplot() %>%
#'    symbols(x=airquality$Wind, y=airquality$Temp, 
#'        circles=airquality$Ozone, add=TRUE) %>%
#'    title(main="Ozone", xlab="Wind", ylab="Temperature")
#' gs
#' 
#' gs <- gsplot() %>%
#'    symbols(x=c(1,2,3), y=c(1,1,1), 
#'        thermometers=matrix(c(1,2,3, 4,4,4, 
#'        0.5,0.25,0.75), ncol=3), fg="red", 
#'        add=TRUE) %>%
#'    title(xlab="", ylab="", xlim=c(0,4))
#' gs
symbols <- function(object, ...) {
  override("graphics", "symbols", object, ...)
}

symbols.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  set_window_args(object, fun.name="symbols", ..., legend.name=legend.name, side=side)
}

