#' gsplot
#'
#' Class stuff
#'
#' @param x list
#' @return gsplot 
#' @export
#' @examples
#' gsplot(list())
gsplot <- function(x){
  UseMethod("gsplot", x)
  invisible(x)
}

#' @export
gsplot.list <- function(x){
  class(x) <- "gsplot"
  invisible(x) 
}
