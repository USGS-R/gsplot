#' gsplot grid
#'
#' Adds grid lines to the plot background. See \code{\link[graphics]{grid}} for more details.
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments
#' 
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, xlim=c(0,NA),ylim=c(0,NA),
#'             col="blue", pch=18, legend.name="Points")
#' gsNew <- lines(gsNew, c(3,4,3), c(2,4,6), legend.name="Lines")
#' gsNew <- grid(gsNew) 
#' gsNew <- abline(gsNew, b=1, a=0, legend.name="1:1")
#' gsNew <- legend(gsNew, location="topleft",title="Awesome!")
#' gsNew
grid <- function(object, ...) {
  override("graphics", "grid", object, ...)
}


grid.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  set_window_args(object, fun.name='grid', ..., legend.name=legend.name, side=side, def.funs = graphics::grid)
}