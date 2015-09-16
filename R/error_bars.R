#' gsplot error bars
#'
#' Creates vertical and horizontal error bars around plot points. 
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
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
#'    error_bar(5, 5, y.high=1) 
#' gs 
error_bar <- function(object, ...) {
  override("gsplot", "error_bar", object, ...)
}


error_bar.gsplot <- function(object, x, y, y.high=0, y.low=0, x.high=0, x.low=0, 
                             epsilon=0.1, ..., legend.name=NULL, side=c(1,2)){
  
  y.high[is.na(y.high)] <- 0
  y.low[is.na(y.low)] <- 0
  x.high[is.na(x.high)] <- 0
  x.low[is.na(x.low)] <- 0
  
  if(!all(y.low == 0)){
    y.low.coord <- y-y.low
    errorIndex <- (y-y.low.coord) != 0
    y.low.coord <- y.low.coord[errorIndex]
    y.error <- y[errorIndex]
    x.error <- x[errorIndex]
    object <- arrows(object, x0=x.error, y0=y.error, x1=x.error, y1=y.low.coord, length=epsilon, angle=90, ...)
  }
  
  if(!all(y.high == 0)){
    y.high.coord <- y+y.high
    errorIndex <- (y-y.high.coord) != 0
    y.high.coord <- y.high.coord[errorIndex]
    y.error <- y[errorIndex]
    x.error <- x[errorIndex]
    object <- arrows(object, x0=x.error, y0=y.error, x1=x.error, y1=y.high.coord, length=epsilon, angle=90, ...)
  }
  
  if(!all(x.low == 0)){
    x.low.coord <- x-x.low
    errorIndex <- (x-x.low.coord) != 0
    x.low.coord <- x.low.coord[errorIndex]
    x.error <- x[errorIndex]
    y.error <- y[errorIndex]
    object <- arrows(object, x0=x.error, y0=y.error, x1=x.low.coord, y1=y.error, length=epsilon, angle=90, ...)
  }
  
  if(!all(x.high == 0)){
    x.high.coord <- x+x.high
    errorIndex <- (x-x.high.coord) != 0
    x.high.coord <- x.high.coord[errorIndex]
    x.error <- x[errorIndex]
    y.error <- y[errorIndex]
    object <- arrows(object, x0=x.error, y0=y.error, x1=x.high.coord, y1=y.error, length=epsilon, angle=90, ...)
  }

  return(object)
    
}

error_bar.default <- function(x, y, y.high, y.low, x.high, x.low, epsilon=0.1, ...){
  return()
}


