#' gsplot barplot
#'
#' Create a bar plot. See \code{\link[graphics]{barplot}} for more details. 
#'
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @details Additional graphical parameter inputs: 
#' \itemize{
#'  \item{\code{height}} {}
#'  \item{\code{width}} {}
#'  \item{\code{space}} {}
#'  \item{\code{names.arg}} {}
#'  \item{\code{beside}} {}
#'  \item{\code{horiz}} {}
#'  \item{\code{axisnames}} {}
#'  \item{\code{cex.axis}} {}
#'  \item{\code{cex.names}} {}
#'  \item{\code{inside}} {}
#'  \item{\code{offset}} {}
#'  \item{\code{add}} {}
#'  \item{\code{legend.name}} {name that appears in the legend, see \code{\link{legend}} for more legend parameters}
#'  }
#'    
#' @rdname barplot
#' @export
#' @examples
#' gs <- gsplot() %>%
#'    barplot(c(21,15,10,18))
#' gs
#' 
barplot <- function(object, ...) {
  override("graphics", "barplot", object, ...)
}

barplot.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  fun.name <- 'barplot'
  object <- set_window_args(object, fun.name=fun.name, ..., legend.name=legend.name, side=side, def.funs=graphics::barplot.default)
  object <- set_legend_args(object, fun.name=fun.name, ..., legend.name=legend.name)
}
