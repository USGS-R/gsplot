#' gsplot axis
#'
#' Axis stuff
#'
#' @param object gsplot object
#' @param \dots ...  Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot(list())
#' gsNew <- axis(gs, side=1)
#' gsNew <- axis(gsNew, side=2)
#' gsNew
axis <- function(object, ...) {
  overrideGraphics("axis", object, ...)
}

axis.gsplot <- function(object, side=NA, at=NULL, labels=TRUE,
                        tick=TRUE, line=NA, pos=NA, outer=FALSE, ...) {
  object <- append(object, list(axes = list(side = side, at=at, labels=labels,
                                tick=tick, line=line, pos=pos, outer=outer,...)))
  return(gsplot(object))
}