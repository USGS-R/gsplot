#' set up the plot frame/window for a side
#'
#'@param object a side object
#'@param side the side number
#'@keywords internal
set_frame <- function(object, side){
  if (is.numeric(side) && length(side) == 1){
    plot.window(xlim = lim(object, side), ylim = lim(object, side), log = ifelse(logged(object, side), as.axis(side),''))
  } else if (is.character(side)){
    view.name <- side
    plot.window(xlim = xlim(object, as.x_side(view.name)), 
                ylim = ylim(object, as.y_side(view.name)), 
                log = as.log(object, view.name))
  } else {
    stop(side, ' type not supported (', class(side) ,')')
  }
}