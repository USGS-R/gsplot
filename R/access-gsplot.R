

#' xlim for gsplot
#' 
#' get the xlim for views in gsplot object
#' 
#' @param object a gsplot object
#' @param side which side(s) to use
#' 
#' @export
xlim <- function(object, side) UseMethod("xlim")

#' @export
xlim.gsplot <- function(object, side=NULL){

  if (!is.null(side))
    views <- object[views_with_side(views(object), side)]
  else 
    views <- views(object)
  names = unname(sapply(views, function(x) paste0('side.',x$window$side[1])))
  unique(lapply(views, function(x) x$window$xlim)) %>% 
    setNames(unique(names))
}

#' ylim for gsplot
#' 
#' get the ylim for views in gsplot object
#' 
#' @param object a gsplot object
#' @param side which side(s) to use
#' 
#' @export
ylim <- function(object, side) UseMethod("ylim")

#' @export
ylim.gsplot <- function(object, side=NULL){
  if (!is.null(side))
    views <- object[views_with_side(views(object), side)]
  else 
    views <- views(object)
  names = unname(sapply(views, function(x) paste0('side.',x$window$side[2])))
  unique(lapply(views, function(x) x$window$ylim)) %>% 
    setNames(unique(names))
}

#' log for gsplot
#' 
#' get the log for views in gsplot object
#' 
#' @name logged
#' @param object a gsplot object
#' @param side which side(s) to use (returns logical)
#' @export
logged <- function(object, side) UseMethod('logged')

#' @rdname logged
#' @export
logged.gsplot <- function(object, side=NULL){
  
  is.logged <- function(window, side){
    log = window$log
    if (side %% 2 == 0){ # is y
      grepl(pattern = 'y',log)
    } else {
      grepl(pattern = 'x',log)
    }
  }
  if (!is.null(side)){
    sapply(side, function(x) is.logged(object[[views_with_side(views(object), side=x)[1]]]$window, x))
  } else {
    names = unname(sapply(views(object), function(x) paste0('side.',paste(x$window$side[1:2],collapse='.'))))
    lapply(views(object), function(x) x$window$log) %>% 
      setNames(names)
  }
}