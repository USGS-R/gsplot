

#' xlim for gsplot
#' 
#' get the xlim for views in gsplot object
#' 
#' @param object a gsplot object
#' 
#' @export
xlim <- function(object) UseMethod("xlim")

#' @export
xlim.gsplot <- function(object){

  lapply(views(object), function(x) x$window$xlim)
}

#' ylim for gsplot
#' 
#' get the ylim for views in gsplot object
#' 
#' @param object a gsplot object
#' 
#' @export
ylim <- function(object) UseMethod("ylim")

#' @export
ylim.gsplot <- function(object){
  
  lapply(views(object), function(x) x$window$ylim)
}

#' log for gsplot
#' 
#' get the log for views in gsplot object
#' 
#' @aliases log
#' @name log
#' @rdname log
#' @param x a gsplot object
#' @param base not used
#' @export
log.gsplot <- function(x,base){
  
  lapply(views(x), function(x) x$window$log)
}