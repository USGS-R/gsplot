

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
  side.lim(object, side, 'x')
}

side.lim <- function(object, side, axis = c('x','y')){
  axis = match.arg(axis)
  side.names <- names(sides(object))
  if (!is.null(side))
    side.names <- as.side_name(side)
  else {
    sides <- as.side(names(sides(object)))
    if (axis == 'y')
      use.sides <- sides %% 2 == 0
    else 
      use.sides <- !sides %% 2 == 0
    side.names <- as.side_name(sides[use.sides])
  }
  
  lims <- lapply(side.names, function(x) object[[x]]$lim) %>% 
    setNames(side.names)
  if (!is.null(side) && length(side==1))
    lims <- lims[[1]]
  return(lims)
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
  side.lim(object, side, 'y')
}

#' log for gsplot
#' 
#' get the log for views in gsplot object
#' 
#' @name logged
#' @param object a gsplot object
#' @param side which side(s) to use (returns logical)
#' @examples 
#' gs <- gsplot() %>%
#'    points(1, 2, legend.name="Cool points", xlim=c(0,NA)) %>%
#'    lines(x=1:5, y=1:5, legend.name="Cool lines", ylab='taco night', log='x')
#'    
#' logged(gs, 1)
#' logged(gs)
#' logged(gs, c(1,2))
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
  if (!is.null(side) && length(side) == 1){
    views <- views(object)
    return(sapply(side, function(x) is.logged(views[[tail(views_with_side(views, side=x),1)]]$window, x)))
  } 
  
  if (is.null(side))
    side = as.side(names(sides(object)))
  
  lapply(side, function(x) logged.gsplot(object, x)) %>% 
    setNames(as.side_name(side))
}

as.log <- function(object, view.name){
  x.side <- as.x_side(view.name)
  y.side <- as.y_side(view.name)
  paste0(ifelse(logged(object, x.side),'x',''),ifelse(logged(object, y.side),'y',''))
}

#' Get view information from a gsplot object
#' 
#' get the views in gsplot object
#' 
#' @param object a gsplot object
#' @return data frame with one row per view. Each view has an x side, y side, the log command, and the view index.
#' @export
view_info <- function(object){
  j <- which_views(object)
  viewSides <- sapply(j, function(x) object[[x]][['window']][['side']])
  viewLogs <- sapply(j, function(x) object[[x]][['window']][['log']])
  viewNames <- names(object[j])
  viewInfo <- data.frame(t(rbind(viewSides, viewLogs, j, viewNames)), stringsAsFactors = FALSE)
  
  names(viewInfo) <- c("x","y","log","index","name")
  
  for(dir in c("y","x")){
    dup_index <- which(duplicated(viewInfo[dir]) | duplicated(viewInfo[dir], fromLast=TRUE))
    viewInfo$log[dup_index[-grep(dir,viewInfo$log)]] <- paste0(dir,viewInfo$log[dup_index[-grep(dir,viewInfo$log)]])
  }
  
  viewInfo[,c("x","y","index")] <- sapply(viewInfo[,c("x","y","index")], as.integer)
  
  i <- which(names(object) %in% 'axis')
  defined.sides <- sapply(i, function(x) object[[x]][['arguments']][['side']])
  view.sides.drawn <- NULL

  viewInfo$x.side.defined.by.user <- viewInfo$x %in% defined.sides
  viewInfo$y.side.defined.by.user <- viewInfo$y %in% defined.sides
  
  return(viewInfo)
}