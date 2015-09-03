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
  
  try.angle <- function() {
    x1 = x[i] + length[i] * xrange * cos(2*pi*(angle.loop/360));
    y1 = y[i] + length[i] * yrange * sin(2*pi*(angle.loop/360));
    return(c(x1,y1))
  }
  
  num.callouts <- max(length(x), length(y)) #determine how many points are given for callouts
  if (length(angle) < num.callouts) {angle <- rep(angle, num.callouts)} #duplicate angles if only one given
  if (length(length) < num.callouts) {length <- rep(length, num.callouts)} #duplicate lengths if only one given
  if (length(x[!is.na(x)])==1) {x <- rep(x[1], num.callouts)}  #duplicate x values if only one given
  if (length(y[!is.na(y)])==1) {y <- rep(y[1], num.callouts)}  #duplicate y values if only one given
  x1 <- c()
  y1 <- c()
  pos <- c()
  
  for (i in 1:num.callouts) {
    
    angle.loop <- angle[i]
    
    if (angle.loop=='auto') {
      angle.loop <- 30
      x.y <- try.angle()
      
      if(x.y[2] > y.usr[2]){
        angle.loop <- 330
        x.y <- try.angle()
        if(x.y[1] > x.usr[2]){angle.loop <- 210}
      }
      
      if(x.y[1] > x.usr[2]) {
        angle.loop <- 210
        x.y <- try.angle()
        if(x.y[2] < y.usr[1]){angle.loop <- 150}
      }
    }
    
    stopifnot(angle.loop >= 0, angle.loop <= 360)
    x.y <- try.angle()
    x1[i] <- x.y[1]
    y1[i] <- x.y[2]
    
    if (angle.loop >= 315 | angle.loop <= 45){
      pos[i] = 4
    } else if (angle.loop > 45 & angle.loop <= 135) {
      pos[i] = 3
    } else if (angle.loop > 135 & angle.loop <= 225){
      pos[i] = 2
    } else {
      pos[i] = 1
    }
    
  }
  
  segments(x0=x, y0=y, x1=x1, y1=y1, ...)
  text(x=x1, y=y1, labels=labels, pos=pos,...)
  
}
