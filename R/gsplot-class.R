#' gsplot
#'
#' Used to change the class of inputs to "gsplot".
#'
#' @param x list
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return gsplot 
#' @export
#' @examples
#' gsplot() 
gsplot <- function(x = NULL, ...) UseMethod("gsplot")

#' @export
gsplot.default <- function(...) {
  gsplot.list(list(par=list(...)))
}

#' @export
gsplot.list <- function(x){
  class(x) <- "gsplot"
  invisible(x) 
}
