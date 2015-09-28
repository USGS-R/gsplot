#' gsplot abline
#'
#' Creates straight lines on the existing gsplot object. See \code{\link[graphics]{abline}} for more details. 
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
  object <- set_window_args(object, fun.name='abline', ..., legend.name=legend.name, side=side, def.funs=c(graphics::abline, graphics::plot.xy))
  views <- lapply(object[which(names(object)=="view")], function(x) {all(x$window$side == side)})
  correctView <- object[[which(unname(unlist(views)))]]
  current.args <- correctView[[which(names(correctView) %in% 'window') - 1]]
  set_legend_args(object, fun.name='abline', current.args)
}