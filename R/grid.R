#' gsplot grid
#'
#' Adds grid lines to the plot background. See \code{\link[graphics]{grid}} for more details.
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments
#' 
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, xlim=c(0,NA),ylim=c(0,NA),
#'             col="blue", pch=18, legend.name="Points")
#' gsNew <- lines(gsNew, c(3,4,3), c(2,4,6), legend.name="Lines")
#' gsNew <- grid(gsNew) 
#' gsNew <- abline(gsNew, b=1, a=0, legend.name="1:1")
#' gsNew <- legend(gsNew, location="topleft",title="Awesome!")
#' gsNew
#' 
#' x.date <- seq(as.Date("2000-01-01"), by = "month", length.out = 10)
#' gs <- gsplot() %>%
#'        points(x.date,1:10) %>%
#'        lines(6:14,6:14,side=c(3,4)) %>%
#'        grid(side=c(3,4))
#' gs
#' 
#' gs <- gsplot() %>%
#'        points(1:10,1:10) %>%
#'        axis(side=1, at=seq(1,10,length.out=18),las=3) %>%
#'        axis(side=3, labels=FALSE) %>%
#'        grid(side=c(1,2),col="green") %>%
#'        grid(side=c(3,4))
#' gs
#' 
#' gs <- gsplot() %>%
#'       points(x=seq.Date(as.Date("2000-01-01"),as.Date("2010-01-01"),length.out = 20),
#'              y=1:20,axes=FALSE) %>%
#'      grid()
#' gs
#'
#' gs <- gsplot() %>% 
#'      points(x=1:10, y=1:10) %>% 
#'      grid(lty=3, col="gray") %>%
#'      axis(side=2, reverse=TRUE) 
#' gs
grid <- function(object, ...) {
  override("graphics", "grid", object, ...)
}

grid.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  
  fun.name='grid'
  object <- gather_function_info(object, fun.name, ..., legend.name=legend.name, side=side)
  return(object)
}

draw_custom_grid <- function(object, view.name){
   
 
  window = object[[view.name]][['window']]
  
  view.name <- names(object[view.name])
  side.names <- as.side_name(view.name)
  
  grid.args <- set_args("grid", object[[view.name]][['grid']], package = "graphics")
  grid.args <- remove_field(grid.args, "equilogs")
  at <- list()
  for (side.name in side.names){
    usr.at <- axis_axTicks(object, as.side(side.name))
    if (is.null(usr.at))
      at[[side.name]] <- grid_axTicks(object, as.side(side.name))
    else
      at[[side.name]] <- usr.at
  }
  
  abline(h=at[[as.y_side_name(view.name)]], v=at[[as.x_side_name(view.name)]], grid.args)
    
}

#' get the locations for ticks for custom grid
#' 
#' @param object a gsplot object
#' @param side a numeric side
#' @return a numeric vector of locations for ticks
#' @details grid_axTicks uses different methods to get tick locations 
#' depending on the class of the data on that \code{side}. For numeric data,
#' \code{\link[graphics]{axTicks}}, for other data (only tested Date and POSIX), 
#' \code{\link[graphics]{axis.Date}} and \code{\link[graphics]{axis.POSIXct}} are used,
#' but the actual plotting of ticks is suppressed. 
#' This function is really hard to test because it uses the actual plot to get tick locations,
#' so it is hard to isolate. 
#' @keywords internal
grid_axTicks <- function(object, side){
  
  lim <- lim(object, side)
  if(is.numeric(lim)){
    at <- axTicks(side)
  } else{
    fun <- getFromNamespace(paste0('axis.',class(lim)[1L]),'graphics')
    at <- fun(side, x = lim, lwd=0, lwd.ticks=0, labels = FALSE)
  }
  return(at)
}

#' get the user-specified locations for axes ticks for a side
#' 
#' @param object a gsplot object
#' @param side a numeric side
#' @return a series of ticks as a vector, or NULL if they weren't specified
#' @keywords internal
axis_axTicks <- function(object, side){
  i <- which(names(object) %in% 'axis')
  defined.sides <- sapply(i, function(x) object[[x]][['arguments']][['side']])
  usr.at <- NULL
  if(side %in% defined.sides){
    axes.index <- i[defined.sides == side]
    usr.at <- object[axes.index][['axis']][['arguments']][['at']]
  }
  return(usr.at)
}
