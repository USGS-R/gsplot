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
#' gsNew <- grid(gsNew, legend.name="Grid") 
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
grid <- function(object, ...) {
  override("graphics", "grid", object, ...)
}

grid.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  
  object <- set_window_args(object, fun.name='grid', ..., legend.name=legend.name, side=side, def.funs = graphics::grid)
  
}

draw_custom_grid <- function(object, index){
   
  i <- which(names(object) %in% 'axis')
  definded.sides <- sapply(i, function(x) object[[x]][['arguments']][['side']])

  view.info <- view_info(object)
  view.info <- view.info[index == view.info$index,]
  
  grid.args <- set_args("grid",object[[index]][['grid']], package = "graphics")
  
  if(view.info$x.side.defined.by.user){
    axes.index <- i[definded.sides == view.info$x]
    x.at <- object[axes.index][['axis']][['arguments']][['at']]
    if(length(x.at) == 0){
      x.at <- axTicks(view.info$x)
    }
  } else {
    x.at <- axTicks(view.info$x)
  }
  
  if(view.info$y.side.defined.by.user){
    axes.index <- i[definded.sides == view.info$y]
    y.at <- object[axes.index][['axis']][['arguments']][['at']]
    if(length(y.at) == 0){
      y.at <- axTicks(view.info$y)
    }
  } else {
    y.at <- axTicks(view.info$y)
  }
  
  abline(h=y.at, v=x.at, grid.args)
    
}