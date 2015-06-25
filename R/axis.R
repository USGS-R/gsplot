#' gsplot axis
#'
#' Axis stuff
#'
#' @param object gsplot object
#' @param side integer vector
#' @param \dots ...  Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot(list())
#' gsNew <- axis(gs, side=1)
#' gsNew <- axis(gsNew, side=2)
axis <- function(object, ...) {
  overrideGraphics("axis", object, ...)
}

axis.gsplot <- function(object, side=NA, ...) {
  object <- append(object, list(axes = list(side = side, ...)))
  return(gsplot(object))
}