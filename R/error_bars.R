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
#'  \item{\code{y.high, y.low}} {the y-value specifying the error above the point (high) and below the point (low)}
#'  \item{\code{x.high, x.low}} {the x-value specifying the error above the point (high) and below the point (low)}
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
#' gsNew <- error_bar(gsNew, 1:3, y=c(3,1,2), y.high=c(0.5,0.25,1), y.low=0.1)
#' gsNew <- error_bar(gsNew, x=1:3, y=c(3,1,2), x.low=c(.2,NA,.2), x.high=.2, col="red",lwd=3)
#' gsNew <- title(gsNew, "Graphing Fun")
#' gsNew
#' 
#' yData <- rnorm(100,mean=10000, sd=1000) 
#' gs <- gsplot() %>%
#'    points(1:100, yData, log="y") %>%
#'    error_bar(50:60, yData[50:60], y.high=250) 
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(1:10, 1:10) %>%
#'    error_bar(5, 5, y.high=1, col="green") 
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
#' @param y.high offset up
#' @param y.low offset down
#' @param x.high offset right
#' @param x.low offset left
#' @param epsilon width of bar in relative units
#' @param \dots additional arguments passed to \code{\link[graphics]{arrows}}
#' @export
#' @examples 
#' plot(1:10, 1:10)
#' error_bar(5, 5, y.high=1, col="green")
error_bar.default <- function(x, y, y.high=0, y.low=0, x.high=0, x.low=0, epsilon=0.1, ...){
  
  data.list <- calculate_error_bars(x=x,y=y,y.high=y.high,y.low=y.low,x.high=x.high, x.low=x.low, epsilon=epsilon)
  
  if(length(data.list[["y.low"]]) > 1){
    graphics::arrows(x0=data.list[["y.low"]]$x0, 
           y0=data.list[["y.low"]]$y0,
           x1=data.list[["y.low"]]$x1,
           y1=data.list[["y.low"]]$y1, 
           length=data.list[["y.low"]]$length, angle=90, ...)
  }
  
  if(length(data.list[["y.high"]]) > 1){
    graphics::arrows(x0=data.list[["y.high"]]$x0, 
           y0=data.list[["y.high"]]$y0,
           x1=data.list[["y.high"]]$x1,
           y1=data.list[["y.high"]]$y1, 
           length=data.list[["y.high"]]$length, angle=90, ...)
  }
  
  if(length(data.list[["x.low"]]) > 1){
    graphics::arrows(x0=data.list[["x.low"]]$x0, 
           y0=data.list[["x.low"]]$y0,
           x1=data.list[["x.low"]]$x1,
           y1=data.list[["x.low"]]$y1, 
           length=data.list[["x.low"]]$length, angle=90, ...)
  }
  
  if(length(data.list[["x.high"]]) > 1){
    graphics::arrows(x0=data.list[["x.high"]]$x0, 
           y0=data.list[["x.high"]]$y0,
           x1=data.list[["x.high"]]$x1,
           y1=data.list[["x.high"]]$y1, 
           length=data.list[["x.high"]]$length, angle=90, ...)
  }

  return()
}

calculate_error_bars <- function(x, y, y.high=0, y.low=0, x.high=0, x.low=0, epsilon=0.1, ...){
  y.high[is.na(y.high)] <- 0
  y.low[is.na(y.low)] <- 0
  x.high[is.na(x.high)] <- 0
  x.low[is.na(x.low)] <- 0
  
  data.list <- rep(list(list()), 4) 
  names(data.list) <- c("y.low","y.high","x.low","x.high")
  
  if(!all(y.low == 0)){
    y.low.coord <- y-y.low
    errorIndex <- (y-y.low.coord) != 0
    y.low.coord <- y.low.coord[errorIndex]
    y.error <- y[errorIndex]
    x.error <- x[errorIndex]
    data.list[["y.low"]] <- list(x0=x.error, y0=y.error, x1=x.error, y1=y.low.coord, 
           length=epsilon, angle=90)
  }
  
  if(!all(y.high == 0)){
    y.high.coord <- y+y.high
    errorIndex <- (y-y.high.coord) != 0
    y.high.coord <- y.high.coord[errorIndex]
    y.error <- y[errorIndex]
    x.error <- x[errorIndex]
    data.list[["y.high"]] <- list(x0=x.error, y0=y.error, x1=x.error, y1=y.high.coord, length=epsilon, 
           angle=90)
  }
  
  if(!all(x.low == 0)){
    x.low.coord <- x-x.low
    errorIndex <- (x-x.low.coord) != 0
    x.low.coord <- x.low.coord[errorIndex]
    x.error <- x[errorIndex]
    y.error <- y[errorIndex]
    data.list[["x.low"]] <- list(x0=x.error, y0=y.error, x1=x.low.coord, y1=y.error, length=epsilon, 
           angle=90)
  }
  
  if(!all(x.high == 0)){
    x.high.coord <- x+x.high
    errorIndex <- (x-x.high.coord) != 0
    x.high.coord <- x.high.coord[errorIndex]
    x.error <- x[errorIndex]
    y.error <- y[errorIndex]
    data.list[["x.high"]] <- list(x0=x.error, y0=y.error, x1=x.high.coord, y1=y.error, length=epsilon, 
           angle=90)
  }

  return(data.list)
}
