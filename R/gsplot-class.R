#' gsplot
#'
#' Class stuff
#'
#' @param x list
#' @return gsplot 
#' @export
#' @importFrom utils getFromNamespace
#' @importFrom stats setNames
#' @importFrom methods existsFunction
#' @examples
#' gsplot()
gsplot <- function(x=list()){
  UseMethod("gsplot", x)
}

#' @export
gsplot.list <- function(x=list()){
  class(x) <- "gsplot"
  invisible(x) 
}
