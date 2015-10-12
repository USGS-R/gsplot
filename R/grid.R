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
#' gsNew <- grid(gsNew, legend.name="Grid") 
#' gsNew <- abline(gsNew, b=1, a=0, legend.name="1:1")
#' gsNew <- legend(gsNew, location="topleft",title="Awesome!")
#' gsNew
#' 
#' gs <- gsplot() %>%
#'        points(1:10,1:10) %>%
#'        lines(6:14,6:14,side=c(3,4)) %>%
#'        grid(side=c(3,4))
#' gs
#' 
#' gs <- gsplot() %>%
#'        points(1:10,1:10) %>%
#'        axis(side=1, at=seq(1,10,length.out=18)) %>%
#'        grid()
#' gs
grid <- function(object, ...) {
  override("graphics", "grid", object, ...)
}

grid.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  
  object <- set_window_args(object, fun.name='grid', ..., legend.name=legend.name, side=side, def.funs = graphics::grid)
  
}