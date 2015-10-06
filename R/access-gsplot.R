

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

  names = unname(sapply(views(object), function(x) paste0('side.',x$window$side[1])))
  lapply(views(object), function(x) x$window$xlim) %>% 
    setNames(names)
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
  
  names = unname(sapply(views(object), function(x) paste0('side.',x$window$side[2])))
  lapply(views(object), function(x) x$window$ylim) %>% 
    setNames(names)
}

#' log for gsplot
#' 
#' get the log for views in gsplot object
#' 
#' @aliases log
#' @name log
#' @rdname log
#' @param object a gsplot object
#' @param side which side(s) to use (returns logical)
#' @export
log <- function(object, side) {
  if (is.gsplot(object))
    UseMethod('log')
  base::log(x=object, base=side)  
}

#' @export
log.gsplot <- function(x, side=NULL){
  names = unname(sapply(views(x), function(x) paste0('side.',paste(x$window$side[1:2],collapse='.'))))
  logs = lapply(views(x), function(x) x$window$log) %>% 
    setNames(names)
  
  if (!is.null(side)){
    warning('not yet implemented. Will return boolean for side matches')
  }
  return(logs)
}