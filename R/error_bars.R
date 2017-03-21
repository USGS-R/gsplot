#' gsplot error bars
#'
#' Creates vertical and horizontal error bars around plot points. 
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @details Additional graphical parameter inputs:
#' \itemize{
#'  \item{\code{x, y}} {location of error_bar origin}
#'  \item{\code{offset.up, offset.down}} {the y-value specifying the error above the point (high) and below the point (low)}
#'  \item{\code{offset.right, offset.left}} {the x-value specifying the error above the point (high) and below the point (low)}
#'  \item{\code{epsilon}} {width of the end of the error bar (in inches)}
#'  \item{\code{legend.name}} {name that appears in the legend, see \code{\link{legend}} for more legend parameters}
#' } 
#'
#' @rdname error_bar
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=c(3,1,2), x=1:3, xlim=c(0,NA),ylim=c(0,NA),
#'             col="blue", pch=18, legend.name="Points")
#' gsNew <- lines(gsNew, c(3,4,3), c(2,4,6), legend.name="Lines")
#' gsNew <- abline(gsNew, b=1, a=0, legend.name="1:1")
#' gsNew <- legend(gsNew, location = "topleft",title="Awesome!")
#' gsNew <- grid(gsNew)
#' gsNew <- error_bar(gsNew, 1:3, y=c(3,1,2), offset.up=c(0.5,0.25,1), offset.down=0.1)
#' gsNew <- error_bar(gsNew, x=1:3, y=c(3,1,2), offset.left=c(.2,NA,.2), offset.right=.2, col="red",lwd=3)
#' gsNew <- title(gsNew, "Graphing Fun")
#' gsNew
#' 
#' yData <- rnorm(100,mean=10000, sd=1000) 
#' gs <- gsplot() %>%
#'    points(1:100, yData, log="y") %>%
#'    error_bar(50:60, yData[50:60], offset.up=250) 
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(1:10, 1:10) %>%
#'    error_bar(5, 5, offset.up=1, col="green") 
#' gs 
error_bar <- function(object, ...) {
  override("gsplot", "error_bar", object, ...)
}


error_bar.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){

  
  fun.name='error_bar'

  object <- gather_function_info(object, fun.name, ..., legend.name=legend.name, side=side)
  arguments <- filter_arguments(fun.name, ..., custom.config = object[["global"]][["config"]][["config.file"]], side=side)

  data.list <- do.call(calculate_error_bars, arguments[["call.args"]]$error_bar)
  data.list <- data.list[sapply(data.list, length) != 0]
  
  for(i in names(data.list)){
    object <- modify_side(object, data.list[[i]], side=side)
  }
  
  return(object)
  
}

#' create error bars
#' 
#' @param x location in x
#' @param y location in y
#' @param offset.up offset up
#' @param offset.down offset down
#' @param offset.right offset right
#' @param offset.left offset left
#' @param epsilon width of bar in relative units
#' @param \dots additional arguments passed to \code{\link[graphics]{arrows}}
#' @export
#' @examples 
#' plot(1:10, 1:10)
#' error_bar(5, 5, offset.up=1, col="green")
error_bar.default <- function(x, y, offset.up=0, offset.down=0, offset.right=0, offset.left=0, epsilon=0.1, ...){
  
  data.list <- calculate_error_bars(x=x,y=y,offset.up=offset.up,offset.down=offset.down,offset.right=offset.right, offset.left=offset.left, epsilon=epsilon)
  
  if(length(data.list[["offset.down"]]) > 1){
    graphics::arrows(x0=data.list[["offset.down"]]$x0, 
           y0=data.list[["offset.down"]]$y0,
           x1=data.list[["offset.down"]]$x1,
           y1=data.list[["offset.down"]]$y1, 
           length=data.list[["offset.down"]]$length, angle=90, ...)
  }
  
  if(length(data.list[["offset.up"]]) > 1){
    graphics::arrows(x0=data.list[["offset.up"]]$x0, 
           y0=data.list[["offset.up"]]$y0,
           x1=data.list[["offset.up"]]$x1,
           y1=data.list[["offset.up"]]$y1, 
           length=data.list[["offset.up"]]$length, angle=90, ...)
  }
  
  if(length(data.list[["offset.left"]]) > 1){
    graphics::arrows(x0=data.list[["offset.left"]]$x0, 
           y0=data.list[["offset.left"]]$y0,
           x1=data.list[["offset.left"]]$x1,
           y1=data.list[["offset.left"]]$y1, 
           length=data.list[["offset.left"]]$length, angle=90, ...)
  }
  
  if(length(data.list[["offset.right"]]) > 1){
    graphics::arrows(x0=data.list[["offset.right"]]$x0, 
           y0=data.list[["offset.right"]]$y0,
           x1=data.list[["offset.right"]]$x1,
           y1=data.list[["offset.right"]]$y1, 
           length=data.list[["offset.right"]]$length, angle=90, ...)
  }

  return()
}

calculate_error_bars <- function(x, y, offset.up=0, offset.down=0, offset.right=0, offset.left=0, epsilon=0.1, ...){
  offset.up[is.na(offset.up)] <- 0
  offset.down[is.na(offset.down)] <- 0
  offset.right[is.na(offset.right)] <- 0
  offset.left[is.na(offset.left)] <- 0
  
  data.list <- rep(list(list()), 4) 
  names(data.list) <- c("offset.down","offset.up","offset.left","offset.right")
  
  if(!all(offset.down == 0)){
    offset.down.coord <- y-offset.down
    errorIndex <- (y-offset.down.coord) != 0
    offset.down.coord <- offset.down.coord[errorIndex]
    y.error <- y[errorIndex]
    x.error <- x[errorIndex]
    data.list[["offset.down"]] <- list(x0=x.error, y0=y.error, x1=x.error, y1=offset.down.coord, 
           length=epsilon, angle=90)
  }
  
  if(!all(offset.up == 0)){
    offset.up.coord <- y+offset.up
    errorIndex <- (y-offset.up.coord) != 0
    offset.up.coord <- offset.up.coord[errorIndex]
    y.error <- y[errorIndex]
    x.error <- x[errorIndex]
    data.list[["offset.up"]] <- list(x0=x.error, y0=y.error, x1=x.error, y1=offset.up.coord, length=epsilon, 
           angle=90)
  }
  
  if(!all(offset.left == 0)){
    offset.left.coord <- x-offset.left
    errorIndex <- (x-offset.left.coord) != 0
    offset.left.coord <- offset.left.coord[errorIndex]
    x.error <- x[errorIndex]
    y.error <- y[errorIndex]
    data.list[["offset.left"]] <- list(x0=x.error, y0=y.error, x1=offset.left.coord, y1=y.error, length=epsilon, 
           angle=90)
  }
  
  if(!all(offset.right == 0)){
    offset.right.coord <- x+offset.right
    errorIndex <- (x-offset.right.coord) != 0
    offset.right.coord <- offset.right.coord[errorIndex]
    x.error <- x[errorIndex]
    y.error <- y[errorIndex]
    data.list[["offset.right"]] <- list(x0=x.error, y0=y.error, x1=offset.right.coord, y1=y.error, length=epsilon, 
           angle=90)
  }

  return(data.list)
}
