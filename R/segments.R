#' gsplot segments
#'
#' Creates line segments in the plot.  See \code{\link[graphics]{segments}} for more details.
#' 
#' @details Additional graphical parameter inputs:
#' \itemize{
#'  \item{\code{x0, y0}} {coordinates for the start of the segment}
#'  \item{\code{x, y}} {coordinates for the end of the segment}
#'  \item{\code{col, lty, lwd}} {parameters describing the color, type, and width of the segment, respectively}
#' }
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, xlim=c(0,NA),ylim=c(0,NA),
#'             col="blue", pch=18, legend.name="Points")
#' gsNew <- lines(gsNew, c(3,4,3), c(2,4,6), legend.name="Lines")
#' gsNew <- abline(gsNew, b=1, a=0, legend.name="1:1")
#' gsNew <- legend(gsNew, location="topleft",title="Awesome!")
#' gsNew <- grid(gsNew)
#' gsNew <- segments(gsNew, x0=2, y0=0.75, y1=1.25)
#' gsNew
segments <- function(object, ...) {
  override("graphics", "segments", object, ...)
}


segments.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  object <- set_window_args(object, fun.name="segments", ..., legend.name=legend.name, side=side, def.funs=graphics::segments)
  views <- lapply(object[which(names(object)=="view")], function(x) {all(x$window$side == set_sides(side))})
  correctView <- object[[which(unname(unlist(views)))]]
  current.args <- correctView[[which(names(correctView) %in% 'window') - 1]]
  set_legend_args(object, fun.name='segments', current.args)
}