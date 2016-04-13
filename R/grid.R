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
#' gs <- gsplot() %>%
#'        points(1:10,1:10) %>%
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
  
  object <- set_window_args(object, fun.name='grid', ..., legend.name=legend.name, side=side, def.funs = graphics::grid)
  
}

draw_custom_grid <- function(object, view.name){
   
  i <- which(names(object) %in% 'axis')
  definded.sides <- sapply(i, function(x) object[[x]][['arguments']][['side']])

  window = object[[view.name]][['window']]
  
  view.name <- names(object[view.name])
  xlim <- xlim(object, side=as.x_side(view.name))
  ylim <- ylim(object, side=as.y_side(view.name))
  view.info <- view_info(object)
  view.info <- view.info[view.name == view.info$name,]
  
  grid.args <- set_args("grid",object[[view.name]][['grid']], package = "graphics")
  
  if(class(xlim) %in% c("numeric","integer")){
    x.at <- axTicks(view.info$x)
  } else if (class(xlim) == "Date"){
    x.at <- axis.Date(view.info$x, xlim)
  } else if (class(xlim) == "POSIXct"){
    x.at <- axis.POSIXct(view.info$x, xlim)
  }
  
  if(view.info$x.side.defined.by.user){
    axes.index <- i[definded.sides == view.info$x]
    x <- object[axes.index][['axis']][['arguments']][['at']]
    if(!is.null(x)){
      x.at <-x
    }
  }
  
  if(class(ylim) %in% c("numeric","integer")){
    y.at <- axTicks(view.info$y)
  } else if (class(ylim) == "Date"){
    y.at <- axis.Date(view.info$y, ylim)
  } else if (class(ylim) == "POSIXct"){
    y.at <- axis.POSIXct(view.info$y, ylim)
  }
    
  if(view.info$y.side.defined.by.user){
    axes.index <- i[definded.sides == view.info$y]
    y <- object[axes.index][['axis']][['arguments']][['at']]
    if(!is.null(y)){
      y.at <- y
    }
  }
  grid.args <- grid.args[names(grid.args) != "equilogs"] 
  abline(h=y.at, v=x.at, grid.args)
    
}