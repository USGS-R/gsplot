#' gsplot callouts
#'
#' Add callout lines and text to a plot.
#' 
#' @details Additional graphical parameter inputs:
#' \itemize{
#'  \item{\code{x}} {numeric value for x-coordinate of callout}
#'  \item{\code{y}} {numeric value for y-coordinate of callout}
#'  \item{\code{labels}} {text to be added to callout}
#'  \item{\code{length}} {relative (percentage of window width and height) distance for callout}
#'  \item{\code{angle}} {callout line angle}
#' }
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#'  
#' @rdname callouts
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, xlim=c(0,3),ylim=c(0,3),
#'             col="blue", pch=18, legend.name="Points")
#' gsNew <- callouts(gsNew, 2, 1, labels='dog')
#' gsNew
callouts <- function(object, ...) {
  override("gsplot", "callouts", object, ...)
}


callouts.gsplot <- function(object, ..., side=c(1,2)){
  
  fun.name <- "callouts"
  to.gsplot <- list(list(arguments = set_args(fun.name, package='gsplot', ...), 
                         gs.config=list(side = side))) %>% 
    setNames(fun.name)
  return(gsplot(append(object, to.gsplot)))
}
#' Default for adding callouts to a plot.
#' 
#' @param x values for callout location
#' @param y values for callout location
#' @param labels text to be added to callout
#' @param length relative (percentage of window width and height) distance for callout
#' @param angle callout line angle
#' 
#' @rdname callouts
#' @export
callouts.default <- function(x, y=NULL, labels=NA, length=0.1, angle='auto', ...){
  
  if (is.null(y)) {
    warning("y=NULL not currently supported in callouts.default")
    return()
  }
  
  stopifnot(angle=='auto' | (angle >= 0 & angle <= 360))
  
  x <- x[!is.na(labels)]
  y <- y[!is.na(labels)]
  labels <- labels[!is.na(labels)]
  
  # // to do: possibly support angle and length as vectors equal in length to x 
  x.usr <- par("usr")[c(1,2)]
  if (par("xlog"))
    x.usr <- 10^x.usr
  y.usr <- par("usr")[c(3,4)]
  if (par("ylog"))
    y.usr <- 10^y.usr
  
  xrange <- diff(x.usr)
  yrange <- diff(y.usr)
  
  if (angle != "auto") {
    x1 <- x + length * xrange * cos(2*pi*(angle/360))
    y1 <- y + length * yrange * sin(2*pi*(angle/360))
  } else {
    auto.angle <- c(30, 330, 150, 210)
    x1 <- sapply(auto.angle, function(a) {
      x + length * xrange * cos(2*pi*(a/360))
    })
    y1 <- sapply(auto.angle, function(a) {
      y + length * yrange * sin(2*pi*(a/360))
    })
    
    good.y1 <- y1 >= y.usr[1] & y1 <= y.usr[2]
    good.x1 <- x1 >= x.usr[1] & x1 <= x.usr[2]
    good.pt <- good.y1 & good.x1
    if (!is.null(dim(good.pt))){
      angle <- auto.angle[apply(good.pt, 1, function(z){
        ifelse(!any(z), 1, min(which(z)))
      })]
    } else { 
      angle <- auto.angle[ifelse(!any(good.pt), 1, min(which(good.pt)))]
    }
    x1 <- x + length * xrange * cos(2*pi*(angle/360))
    y1 <- y + length * yrange * sin(2*pi*(angle/360))
  }
    
  pos <- rep(1, length(angle))  
  pos[angle >= 315 | angle <= 45] <- 4
  pos[angle > 45 & angle <= 135] <- 3
  pos[angle > 135 & angle <= 225] <- 2

  segments(x0=x, y0=y, x1=x1, y1=y1, ...)
  text(x=x1, y=y1, labels=labels, pos=pos,...)
  
}
