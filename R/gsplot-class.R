#' gsplot
#'
#' Used to change the class of inputs to "gsplot".
#'
#' @param x list
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return gsplot 
#' @export
#' @rdname gsplot
#' @examples
#' gsplot() 
gsplot <- function(x = NULL, ...) UseMethod("gsplot")

#' @rdname gsplot
#' @export
gsplot.default <- function(...) {
  par.gsplot(gsplot.list(list()), ...)
}

#' @rdname gsplot
#' @exportMethod gsplot
gsplot.list <- function(x){
  class(x) <- "gsplot"
  invisible(x) 
}
